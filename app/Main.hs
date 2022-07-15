{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


import Lambda_interpreter as L
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import qualified GI.Gtk.Objects.Grid as Gtk
import GI.Gdk (screenGetDefault)
import Control.Exception
import Web.Browser (openBrowser)


main :: IO ()
main = do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setContainerBorderWidth win 10
    Gtk.setWindowTitle win "lambda interpreter"
    Gtk.setWindowDefaultHeight win 400
    Gtk.setWindowDefaultWidth win 800
    Gtk.setWindowResizable win True
   

    prov <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromPath prov "app/style.css"
    screenGetDefault >>= \case
        Just screen -> Gtk.styleContextAddProviderForScreen screen prov 800
        Nothing -> return () 
    

    grid <- Gtk.gridNew

    returntext <- Gtk.labelNew (Just "Enter a Lambda above") 
    Gtk.labelSetJustify returntext Gtk.JustificationLeft 
    Gtk.labelSetXalign returntext (-4)
    Gtk.labelSetSelectable returntext True
    Gtk.widgetSetName returntext "returntext"

   -- webpage <- Gtk.buttonNew
    --Gtk.buttonSetLabel webpage "website"
    --Gtk.on webpage #clicked (do openBrowser "https://shitgaem.online"
                                --return ())
        

    textbox <- Gtk.entryNew
    Gtk.setEntryPlaceholderText textbox "enter lambda"
    Gtk.widgetSetName textbox "input_field"
    Gtk.onEntryActivate textbox $ do
       toreduceIO <- Gtk.getEntryBuffer textbox
       toreduce <- Gtk.getEntryBufferText toreduceIO
       geil <- catch (Gtk.labelSetLabel returntext (T.pack $ L.convert (T.unpack toreduce)))
                     (\e -> do let err = show (e :: L.ParserException)
                               Gtk.labelSetLabel returntext $  T.pack err 
                               return ())
       return ()

    #attach grid textbox 0 0 10 1
    #attach grid returntext 0 2 10 10
    --attach grid webpage 30 0 2 2
    #add win grid

    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    returntext <- Gtk.labelNew (Just "") 
    Gtk.main



