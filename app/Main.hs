{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


import Lambda_interpreter as L
import qualified GI.Gtk as Gtk
import qualified Data.Text as T
import qualified GI.Gtk.Objects.Grid as Gtk

main :: IO ()
main = do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setContainerBorderWidth win 10
    Gtk.setWindowTitle win "lambda interpreter"
    Gtk.setWindowDefaultHeight win 400
    Gtk.setWindowDefaultWidth win 800
    Gtk.setWindowResizable win True
    

    grid <- Gtk.gridNew 

    returntext <- Gtk.labelNew (Just "") 

    textbox <- Gtk.entryNew
    Gtk.setEntryPlaceholderText textbox "enter lambda"
    Gtk.onEntryActivate textbox $ do
       toreduceIO <- Gtk.getEntryBuffer textbox
       toreduce <- Gtk.getEntryBufferText toreduceIO
       geil <- Gtk.labelSetLabel returntext (T.pack $ L.convert (T.unpack toreduce))
       return geil


    #attach grid textbox 0 0 5 1
    #attach grid returntext 1 1 5 1
    #add win grid

    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    returntext <- Gtk.labelNew (Just "") 
    Gtk.main



