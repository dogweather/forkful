---
title:                "अस्थायी फ़ाइल बनाना"
html_title:           "Gleam: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
Temporary file को create करना रोज़मर्रा की programming में एक common task है। यह काम उस समय किया जाता है जब हमारे पास अपनी temporary data को store करने का dedicated space नहीं है, और हम उसे temporary file के रूप में बना कर संग्रहीत करना चाहते हैं।

## कैसे करें?
```Gleam
let { Ok, Err } = file.write_temporary("temp.txt", "Temporary file content")
```

यह कोड temporary file "temp.txt" को create करेगा और उसमें "Temporary file content" लिखेगा। अगर कोई error होता है तो उसे Err में store किया जाएगा। जबकि अगर सब कुछ ठीक हो तो Ok में success message store हो जाएगा।

## गहराई में जाएं
Temporary file को create करने का concept वास्तव में unix-like operating systems से संबंधित है। ये operating systems process से associated हैं। जब process terminate होता है तो उसके साथ associated temporary file भी automatically delete हो जाता है। यह एक efficient way है अगर हम प्रोग्राम में temporary file को साफ़ करने का बार-बार burden से नहीं चाहते हैं।

## और देखें
- [आधिक्य के बारे में जानें](https://github.com/lpil/gleam_stdlib/blob/master/lib/gleam_std/temporary/file.gleam#L88)
- [दो alternatives भी विचार करें](https://github.com/lpil/gleam_stdlib/blob/master/lib/gleam_std/temporary/file.gleam#L15)
- [temporary file को create करने के दूसरे तरीके](https://github.com/lpil/gleam_stdlib/blob/master/lib/gleam_std/temporary/file.gleam#L33)