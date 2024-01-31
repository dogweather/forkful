---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:57:05.630943-07:00
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी की मौजूदगी चेक करना यानि पता करना कि कोई फोल्डर सिस्टम में है या नहीं। प्रोग्रामर्स इसलिए करते हैं ताकि फाइल ऑपरेशन्स बिना एरर के चलें।

## How to: (कैसे करें:)
```Haskell
import System.Directory (doesDirectoryExist)

-- यह हमारा फंक्शन है चेक करने के लिए
checkDirectory :: FilePath -> IO ()
checkDirectory path = do
    exists <- doesDirectoryExist path
    putStrLn $ "Directory " ++ path ++ (if exists then " exists." else " does not exist.")

-- मुख्य फंक्शन जहां हम `checkDirectory` का इस्तेमाल कर रहे
main :: IO ()
main = do
    checkDirectory "/path/to/your/directory"
```
सैंपल आउटपुट:
```
Directory /path/to/your/directory exists.
```
या अगर डायरेक्टरी नहीं है, तो:
```
Directory /path/to/your/directory does not exist.
```

## Deep Dive (गहराई से जानकारी):
- इतिहास: Haskell में `System.Directory` मॉड्यूल लंबे समय से है, जो फाइल सिस्टम ऑपरेशन्स को मैनेज करने देता है।
- विकल्प: `doesFileExist` फंक्शन जांचता है कि कोई फाइल मौजूद है कि नहीं। तीसरे पक्ष के लाइब्रेरीज भी हैं जैसे `Shelly` या `turtle`। 
- कार्यान्वयन: `doesDirectoryExist` सिस्टम कॉल्स का उपयोग करके चेक करता है। ऑपरेटिंग सिस्टम के अनुसार काम करता है, पोर्टेबिलिटी सुनिश्चित करता है।

## See Also (और जानकारी के लिए):
- [System.Directory Documentation](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [Haskell.org](https://www.haskell.org/)
