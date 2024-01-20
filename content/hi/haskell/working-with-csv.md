---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV यानी Comma-Separated Values एक साधारण फॉर्मेट है जो कई सारे डेटा हेतु इस्तेमाल किया जाता है। प्रोग्रामर इसे डेटा को स्टोर, संग्रहित, और साझा करने के लिए इस्तेमाल करते हैं, जैसे कि आँकड़ा विश्लेषण, डेटाबेस अंतर्गतन और मशीन सीखने की प्रक्रियाओं में।

## कैसे करें:

CSV फाइल को पढ़ना, लिखना और पार्स करना Haskell में आसान है। नीचे कुछ उदाहरण हैं:

```haskell
-- CSV फाइल को पढ़ना:
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv

-- | एक CSV फाइल से डेटा पढ़ें और पार्स करें।
readCsvFile :: FilePath -> IO (Either String (Vector (Vector String)))
readCsvFile filePath = do
    csvData <- BL.readFile filePath
    return $ Csv.decode Csv.NoHeader csvData

-- सैंपल फंक्शन कॉल
-- main = do
--   result <- readCsvFile "data.csv"
--   case result of
--     Left err -> putStrLn err
--     Right rows -> print rows

-- CSV फाइल में लिखना:
import qualified Data.Vector as V

-- | CSV फॉर्मेट में डेटा को फाइल में लिखें।
writeCsvFile :: FilePath -> Vector (Vector String) -> IO ()
writeCsvFile filePath csvData = BL.writeFile filePath $ Csv.encode (V.toList csvData)

-- सैंपल डेटा और फंक्शन कॉल
-- main = do
--   let rows = V.fromList [V.fromList ["name", "age"], V.fromList ["Alice", "30"]]
--   writeCsvFile "output.csv" rows
```

उपर दिए गए कोड में `readCsvFile` फंक्शन CSV फाइल को पार्स करता है, और `writeCsvFile` फंक्शन वेक्टर डेटा को CSV फाइल में लिखता है।

## गहराई में जानकारी:

CSV डेटा के विनिमय के लिए एक लोकप्रिय विधि है और यह 1970 के दशक से इस्तेमाल में है। Haskell में `cassava` लाइब्रेरी एक मानक विकल्प है CSV काम करने के लिए। इस लाइब्रेरी में strong typing और efficient parsing के फायदे हैं। वैकल्पिक रूप से, `text` और `bytestring` जैसे निचले स्तर के पैकेज भी डेटा को प्रोसेस कर सकते हैं, पर ज़्यादा कोड लिखने की जरुरत पड़ सकती है।

## यह भी देखें:

- Cassava library documentation: [http://hackage.haskell.org/package/cassava](http://hackage.haskell.org/package/cassava)
- Data.Csv module on Hackage: [http://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html](http://hackage.haskell.org/package/cassava-0.5.2.0/docs/Data-Csv.html)
- Haskell ByteString documentation: [https://hackage.haskell.org/package/bytestring](https://hackage.haskell.org/package/bytestring)
- Haskell Text library on Hackage: [https://hackage.haskell.org/package/text](https://hackage.haskell.org/package/text)