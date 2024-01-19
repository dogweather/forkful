---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों? | What & Why?

कमांड लाइन आर्ग्युमेंट्स पढ़ना मतलब होता है उपयोगकर्ता के द्वारा एक प्रोग्राम को चलाते समय प्रदान की गई जानकारियों को प्राप्त करना।  प्रोग्रामर्स इसे उपयोग में लाते हैं ताकि वे उपयोगकर्ताओं द्वारा प्रदत्त डेटा पर अपने प्रोग्राम को अनुकूलित कर सकें।

## कैसे करें | How to:

Gleam में command line arguments को पढ़ने के लिए `os.args` का उपयोग किया जाता है। यह एक List लौटाता है जिसमें कमांड लाइन से आये हुए arguments होते हैं। 

```Gleam
import gleam/os
import gleam/list

fn main(args: List(String)) {
  let command_args = os.args()
  case command_args {
    [] -> 
      list.from_empty()
    [first | remainder] -> 
      list.from(remainder)
  }
}
```
## गहराई में| Deep Dive

आज के दिन तक जितनी भी कमांड लाइन इंटरफ़ेस हui हैं, उनमें से अधिकांश पहले Unix सिस्टम में विकसित की गई थीं। `os.args` का इस्तेमाल करने के विकल्प के रूप में, आप लाइब्रेरीज़ का इस्तेमाल कर सकते हैं जो बड़े command-line applications के लिए अधिक robust विचारणाओं प्रदान करती हैं। जैसा कि Gleam अभी भी विकास के एक बहुत प्रारंभिक चरण में है, इसका उपयोग भी कम ही होता है। 

## और देखें | See Also

1. Gleam के आधिकारिक [documentation](https://gleam.run/documentation/) में विशेष रूप से command-line arguments के बारे में अधिक।
2. [Erlang in Anger](http://erlang-in-anger.com/), एक मुफ्त eBook की एक महत्वपूर्ण पाठ्यक्रम है जिसमें Erlang (जिसपर Gleam बना हुआ है) के बारे में विस्तृत जानकारी है।