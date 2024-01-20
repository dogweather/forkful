---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Command line arguments वो इनपुट होते हैं जो हम अपने प्रोग्राम को CLI (Command Line Interface) के माध्यम से देते हैं। प्रोग्रामर्स इसे use करते हैं क्योंकि इससे प्रोग्राम को फ्लेक्सिबल और रीयूसेबल बनाया जा सकता है।

## कैसे करें:

आप निम्नलिखित Elixir कोड का उपयोग करके command line arguments पढ़ सकते हैं। 

```Elixir
defmodule MyProgram do
  def main(args) do
    IO.inspect(args)
  end
end
```

आपके या उनके आउटपुट कुछ इस तरह का होगा:

```Elixir
~$ elixir my_program.exs arg1 arg2 arg3
["arg1", "arg2", "arg3"]
```

## गहराई में:

**ऐतिहासिक संदर्भ:** Command line arguments का उपयोग एक्सेक्यूटेबल प्रोग्रामों के लिए shell scripting के दिनों से होता आ रहा है। Elixir के पाठ्यक्रम में भी इसे बड़े महत्व के साथ स्थान दिया गया है।

**विकल्प:** कमांड लाइन आर्गुमेंट्स के अलावा, आप फ़ाइल इनपुट, पर्यावरण चर, और स्टैंडर्ड इनपुट का भी उपयोग कर सकते हैं।

**व्याख्यान विवरण:** Elixir में, `System.argv/0` फ़ंक्शन का उपयोग करके कमांड लाइन आर्गुमेंट्स पढ़े जा सकते हैं।

## देखिए भी:

अधिक जानकारी के लिए, आप निम्नलिखित स्रोतों से जुड़ सकते हैं:

Elixir School ([https://elixirschool.com/hi/lessons/basics/otp-applications/#command-line-args](https://elixirschool.com/hi/lessons/basics/otp-applications/#command-line-args))
Elixir Documentation ([https://hexdocs.pm/elixir/System.html](https://hexdocs.pm/elixir/System.html))