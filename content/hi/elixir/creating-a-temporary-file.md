---
title:                "अस्थायी फाइल बनाना"
date:                  2024-01-20T17:40:09.345062-07:00
model:                 gpt-4-1106-preview
simple_title:         "अस्थायी फाइल बनाना"

category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
अस्थायी फाइल एक ऐसी फ़ाइल है जिसे डाटा को अस्थायी रूप से संग्रहित करने के लिए बनाया जाता है, और बाद में ये स्वतः ही मिट जाती हैं। प्रोग्रामर्स इनका उपयोग अस्थायी डाटा संग्रहण, कैशिंग, या एप्लिकेशन्स के बीच डेटा का संचार करने के लिए करते हैं।

## How to: (कैसे करें:)
```elixir
# Elixir में अस्थायी फ़ाइल बनाने का उदाहरण:

# Filestream पैकेज का उपयोग करें
{:ok, file} = Filestream.tempfile(prefix: "temp_", ext: ".txt")

# फाइल में कुछ लिखें
:ok = IO.binwrite(file, "यह एक अस्थायी फाइल है")

# अस्थायी फ़ाइल का पथ पता करें
IO.puts("अस्थायी फ़ाइल का पथ: #{:file.name(file)}")

# उपयोग के बाद फाइल बंद करें
Filestream.close(file)

# संदर्भ के आधार पर, फ़ाइल स्वतः ही मिट सकती है
```
सैंपल आउटपुट: अस्थायी फ़ाइल का पथ: /tmp/temp_abc123.txt

## Deep Dive (गहराई से जानिए)
अस्थायी फ़ाइलें UNIX जैसे ऑपरेटिंग सिस्टम पर लम्बे समय से उपयोग में हैं। Elixir में, `Filestream` जैसे पैकेज इस कार्य को आसान बनाते हैं। विकल्प में, आप `:os.cmd/1` का उपयोग करके सीधे सिस्टम कमांड्स चला सकते हैं। अस्थायी फ़ाइल कार्यान्वयन में, फ़ाइल OS द्वारा नियंत्रित विशेष डायरेक्टरी में बनती है और इसे आइडेंटिफ़ायर (जैसे कि यूनिक प्रिफिक्स या सफ़िक्स) से चिह्नित किया जाता है। 

## See Also (और जानकारी के लिए)
- Elixir का आधिकारिक दस्तावेज: https://hexdocs.pm/elixir/File.html
- Filestream पैकेज: https://hex.pm/packages/filestream
- अस्थायी फ़ाइलों के बारे में विकिपीडिया: https://en.wikipedia.org/wiki/Temporary_file

नोट: उपलब्ध डाटा और URL के आधार पर जानकारी अपडेट हो सकती है।
