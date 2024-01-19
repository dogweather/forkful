---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# अस्थायी फ़ाइल बनाना - Elixir (वर्तमान संस्करण)

## "क्या और क्यों?"

एक अस्थायी फ़ाइल बनाना मतलब होता है कि आप एक फ़ाइल तब तक बनाते हैं जब तक कि आपकी प्रोग्राम को उसकी आवश्यकता हो, और फिर इसे डिलीट कर देते हैं। यह स्थान और संसाधनों के समाप्त होने को रोकने और इससे बचने का एक अच्छा तरीका हो सकता है।

## "कैसे करें:"

यहां एक सरल Elixir कोड है:

```elixir
{:ok, file} = File.open("temp.txt", [:write])
IO.binwrite(file, "यह एक अस्थायी फ़ाइल है")
File.close(file)
File.rm("temp.txt")
```

## "गहरा डाइव"

1. **ऐतिहासिक प्रसंग**: जब कंप्यूटर की हार्ड डिस्क स्थान की कमी होती थी, तब अस्थायी फ़ाइलें बहुत महत्वपूर्ण होती थीं। आज भी, ये काफी हेल्पफुल होती हैं, खासकर जब डाटा अनेक टाइम्स लिखा जा रहा हो।

2. **विकल्प**: `System.tmp_dir/0` आपको खुद से एक अस्थायी फ़ाइल बनाने की क्षमता देता है। लेकिन यह तब तक पर्सिस्ट नहीं होता जब तक आप खुद इसे हटा नहीं देते।

3. **कार्यान्वयन विवरण**: जब आप File.open का उपयोग करते हैं, तो एलिक्सिर फ़ाइल सिस्टम के साथ निंटरेक्ट करता है। इसके बाद, `File.rm` फ़ाइल को हटा देता है।

## "यह भी देखें"

1. [Elixir School: Files](https://elixirschool.com/en/lessons/advanced/erlang): इस पृष्ठ का आलोचना Elixir में फ़ाइलों के साथ काम करने पर केंद्रित है।

2. [Elixir Lang: File Module](https://hexdocs.pm/elixir/File.html): यहां Elixir में फ़ाइल मॉड्यूल का विमर्श किया गया है।

3. [Erlang: File Handling](http://erlang.org/doc/man/file.html): इस पृष्ठ पर ईरलैंग में फ़ाइल हैंडलिंग का विवरण है, जिसे Elixir ने इनहेरिट किया है।