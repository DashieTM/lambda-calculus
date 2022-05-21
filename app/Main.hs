{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


import Lambda_interpreter as L
import qualified GI.Gtk as Gtk
import qualified Data.Text as T

main :: IO ()
main = do
    Gtk.init Nothing

    win <- Gtk.windowNew Gtk.WindowTypeToplevel
    Gtk.setContainerBorderWidth win 10
    Gtk.setWindowTitle win "lambda interpreter"
    Gtk.setWindowDefaultHeight win 400
    Gtk.setWindowDefaultWidth win 800
    Gtk.setWindowResizable win True

    textbox <- Gtk.entryNew
    Gtk.setEntryPlaceholderText textbox "enter lambda"
    result <- Gtk.onEntryActivate textbox $ do
            toreduceIO <- Gtk.getEntryBuffer textbox
            toreduce <- Gtk.getEntryBufferText toreduceIO
            L.convert (T.unpack toreduce)

    


    #add win textbox

    Gtk.onWidgetDestroy win Gtk.mainQuit
    #showAll win
    Gtk.main



