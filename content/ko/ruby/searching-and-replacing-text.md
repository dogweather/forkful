---
title:                "텍스트 검색 및 교체"
html_title:           "Elixir: 텍스트 검색 및 교체"
simple_title:         "텍스트 검색 및 교체"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

프로그래밍에서 텍스트 검색 및 교체는 문자열 내에서 특정 단어를 찾거나 다른 단어로 교체하는 과정입니다. 이를 통해 데이터 수정, 정리, 정규화 등을 쉽게 수행할 수 있습니다.

## 어떻게 사용하는가:

Ruby에서는 `gsub` 메서드를 사용하여 텍스트 검색 및 교체를 할 수 있습니다. 

```Ruby 
text = '안녕하세요. 저는 Ruby 개발자입니다.'
text.gsub('Ruby', '파이썬') 
=> "안녕하세요. 저는 파이썬 개발자입니다."
```

여기서 'Ruby'를 찾아서 '파이썬'으로 교체했습니다.

## 깊이 파헤쳐보기

텍스트 검색 및 교체는 고대 문자 처리시스템에서부터 존재한 기능입니다. 이 기능은 마이크로소프트 워드와 같은 프로그램에서도 많이 사용됩니다.

Ruby 외의 다른 프로그래밍 언어에서도 이런 방법들이 존재합니다. JavaScript에서는 `replace()`, Python에서는 `replace()` 함수를 사용합니다.

`gsub` 메서드는 내부적으로 정규 표현식을 사용합니다. 이는 텍스트 검색 및 교체를 정확하게 수행하기 위한 강력한 도구입니다. `gsub`는 'Global Substitution'의 줄임말로, 전체 텍스트에서 매칭되는 모든 항목을 교체합니다.

## 참고 자료

- [Ruby Documentation for String#gsub](https://ruby-doc.org/core-2.6.1/String.html#method-i-gsub)
- [Ruby Guide on Regular Expressions](https://ruby-doc.org/core-2.6.1/Regexp.html)
- [JavaScript String Replace Method](https://www.w3schools.com/jsref/jsref_replace.asp)
- [Python String Replace Method](https://docs.python.org/3/library/stdtypes.html#str.replace)