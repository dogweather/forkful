---
date: 2024-01-20 17:40:09.345062-07:00
description: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932\
  \ \u090F\u0915 \u0910\u0938\u0940 \u092B\u093C\u093E\u0907\u0932 \u0939\u0948 \u091C\
  \u093F\u0938\u0947 \u0921\u093E\u091F\u093E \u0915\u094B \u0905\u0938\u094D\u0925\
  \u093E\u092F\u0940 \u0930\u0942\u092A \u0938\u0947 \u0938\u0902\u0917\u094D\u0930\
  \u0939\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\
  \u0928\u093E\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u0914\u0930 \u092C\
  \u093E\u0926 \u092E\u0947\u0902 \u092F\u0947 \u0938\u094D\u0935\u0924\u0903 \u0939\
  \u0940 \u092E\u093F\u091F \u091C\u093E\u0924\u0940 \u0939\u0948\u0902\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0928\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917\u2026"
lastmod: '2024-03-13T22:44:51.770821-06:00'
model: gpt-4-1106-preview
summary: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u090F\
  \u0915 \u0910\u0938\u0940 \u092B\u093C\u093E\u0907\u0932 \u0939\u0948 \u091C\u093F\
  \u0938\u0947 \u0921\u093E\u091F\u093E \u0915\u094B \u0905\u0938\u094D\u0925\u093E\
  \u092F\u0940 \u0930\u0942\u092A \u0938\u0947 \u0938\u0902\u0917\u094D\u0930\u0939\
  \u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\u0928\
  \u093E\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948, \u0914\u0930 \u092C\u093E\
  \u0926 \u092E\u0947\u0902 \u092F\u0947 \u0938\u094D\u0935\u0924\u0903 \u0939\u0940\
  \ \u092E\u093F\u091F \u091C\u093E\u0924\u0940 \u0939\u0948\u0902\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0928\u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917\u2026"
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
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
