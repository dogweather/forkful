---
title:                "C++: 継続的な表現の使用"
simple_title:         "継続的な表現の使用"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Mina-san ni totte no C++ Programming ni tsuite no Blog Post

## Why

C++ wo katsuyou suru ma niwa, regular expression wo tsukau riyuu ga hitsuyou ni narimasu. Regular expression wa, mojiretsu no setsuzoku wo fukakami kara bunmei ni hantaisuru koto ga dekimasu.

## How To

```C++
#include <iostream>
#include <regex>

int main() {
  std::string str = "Watashitachi wa unity-ga suki desu.";
  std::regex reg("unity");
  if (std::regex_search(str, reg)) {
    std::cout << "Watashi-tachi wa Unity wo kanjimasu!" << std::endl;
  } else {
    std::cout << "Watashi-tachi wa Unity wo kanjinai yo." << std::endl;
  }
}
```

**Output:**

Watashi-tachi wa Unity wo kanjimasu!

## Deep Dive

Regular expression wo katsuyou suru to, soteigai na bunkatsu wo motometai toki ni yori rikai kanou ni natte kimasu. Regular expression wa, kanji mojiretsu wo kyouten sareta mondai ni tsuite, kantan ni sorosorowanai fukakai na ketsuron wo heisa suru koto ga dekimasu. Soshite, regex wo tsukatte mo, code no kiraina kazu de saimon wo shori suru koto mo dekimasu.

## See Also

- [Regex tutorial in C++](https://www.tutorialspoint.com/cplusplus/cpp_regular_expressions.htm)
- [Regular expressions in C++](https://en.cppreference.com/w/cpp/regex)
- [Mastering Regular Expressions book](https://www.oreilly.com/library/view/mastering-regular-expressions/9780596528126/)