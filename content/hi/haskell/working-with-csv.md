---
title:                "CSV के साथ काम करना"
html_title:           "Haskell: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप CSV फाइलों के साथ काम करने का बहुत खूबसूरत तरीका ढूंढ रहे हैं? यदि हाँ, तो आप बिल्कुल सही जगह पर हैं। यह हास्केल नामक प्रोग्रामिंग भाषा के द्वारा आसानी से साध्य है और सीएसवी फाइलों को प्रसंस्करण और पढ़ाने को बहुत आसान बनाता है। इसलिए, हम आपको हास्केल में सीएसवी फाइलों के साथ काम करने का आनंद लेने के लिए इस लेख को पढ़ने का आह्वान करते हैं। 

## कैसे करें

हास्केल में CSV फाइलों को पढ़ने और उनका प्रसंस्करण करने के लिए कुछ आसान तरीके हैं। नीचे कुछ कोड उदाहरण हैं जो आपको CSV फाइलों से डेटा पढ़ने और उसे अन्य प्रारूपों में लिखने में मदद कर सकते हैं।

```Haskell
import Text.CSV

-- read a CSV file and print its contents
readCSV :: FilePath -> IO ()
readCSV path = do
    csv <- parseCSVFromFile path
    case csv of
        Left err -> printError err
        Right records -> printRecords records

-- print out any parsing errors
printError :: Show a => a -> IO ()
printError err = putStrLn ("Error: " ++ show err)

-- helper function to print out the records
printRecords :: Show a => [Record a] -> IO ()
printRecords = mapM_ (mapM_ print)
```

उपरोक्त उदाहरण में, हमने `Text.CSV` मोड्यूल को आयात किया है और `readCSV` नामक एक फ़ंक्शन लिखी है जो फाइल का पथ लेता है और उसे प्रोसेस करने के लिए `parseCSVFromFile` फ़ंक्शन का इस्तेमाल करता है। यदि कोई त्रुटि होती है, तो हम `printError` फ़ंक्शन का उपयोग करते हैं जो एरर को मुद्रित करता है, और यदि सब कुछ ठीक है तो हम `printRecords` फ़ंक्शन का इस्तेमाल करते हैं जो सभी रेकॉर्ड मुद्रित करता है