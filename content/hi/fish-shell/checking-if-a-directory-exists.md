---
title:                "डायरेक्टरी मौजूद होने की जाँच करना"
html_title:           "Fish Shell: डायरेक्टरी मौजूद होने की जाँच करना"
simple_title:         "डायरेक्टरी मौजूद होने की जाँच करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

डायरेक्ट्री मौजूद होने की जांच करने से हम यह जान सकते हैं कि क्या हमें उस डायरेक्ट्री में फाइलें खोजने की ज़रूरत है या नहीं।

## कैसे

```Fish Shell``` में डायरेक्ट्री की जांच कैसे करें:
```fish
# स्लैक्स में डायरेक्ट्री का नाम दर्ज करें
set dir_name /home/user/Downloads
# डायरेक्ट्री की जांच करें
if test -d $dir_name
  echo "यह डायरेक्ट्री मौजूद है"
else
  echo "यह डायरेक्ट्री मौजूद नहीं है"
end
```

यदि डायरेक्ट्री मौजूद है, तो आपको "यह डायरेक्ट्री मौजूद है" का संदेश मिलेगा। और अगर यह मौजूद नहीं है, तो आपको "यह डायरेक्ट्री मौजूद नहीं है" का संदेश मिलेगा।

## गहराई तक जाएं

डायरेक्ट्री की जांच कोड में ```test``` एक बिल्ट-इन कमांड है जो फ़ाइलों और डायरेक्ट्री के मौजूद होने को जांचता है। ```-d``` पैरामीटर डायरेक्ट्री के मौजूद होने को जांचता है और अगर डायरेक्ट्री मौजूद है तो उसके साथ संबंधित कोड एक्सीक्यूट होगा। आप भी ```-f``` पैरामीटर का उपयोग कर सकते हैं जो एक फ़ाइल के मौजूद होने को जांचता है।

## देखें भी

- [फ़ाइलें कैसे बनाएं](https://fishshell.com/docs/current/tutorial.html#_creating_files)
- [फ़ाइलों और डायरेक्ट्रीज़ कैसे हटाएं](https://fishshell.com/docs/current/tutorial.html#_removing_files_and_directories)
- [Shell में परिभाषित चर और उनकी कामनाएं](https://fishshell.com/docs/current/tutorial.html#_defined_variables)