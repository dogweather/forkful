---
title:    "Elixir: 문자열을 소문자로 변환하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 이유는 데이터의 일관성을 유지하고, 검색 및 비교 작업을 더 쉽게하기 위해서입니다.

## 하기 방법

```Elixir
# 간단한 방법
String.downcase("ELIXIR") #=> "elixir"

# 변수에 할당하기
original_string = "ELIXIR"
lower_case = String.downcase(original_string) #=> "elixir"

# 조건문과 함께 사용하기
String.downcase("Elixir", "en") #=> "elixir"
String.downcase("Elixir", "ko") #=> "elixir"
```

첫번째 예제는 단순히 `String.downcase/1` 함수를 사용하여 문자열을 소문자로 변환합니다. 두번째 예제는 변수를 사용하여 변환한 문자열을 다룰 수 있도록 합니다. 마지막 예제는 두번째 인자에 언어 코드를 지정하여 해당 언어에 맞는 소문자로 변환할 수 있습니다.

## 더 깊게 들어가기

Elixir에서는 문자열 변환 작업을 위해 내장 함수인 `String.downcase/1` 외에도 더 많은 옵션을 제공합니다. `String.downcase/3` 함수를 사용하면 언어 코드 외에도 유니코드 표준에 따라 변환 작업을 할 수 있습니다. 또한, `String.downcase/3` 함수는 `:locale` 옵션을 통해 사용자가 직접 언어에 맞는 변환 규칙을 전달할 수 있습니다.

## 관련 링크

[《Elixir School》](https://elixirschool.com/ko/lessons/basics/strings/#%EB%AC%B8%EC%9E%90%EC%97%B4-%EB%B3%80%ED%99%98)  
[《Programming Elixir 1.6》](https://doc-kurento.readthedocs.io/en/latest/INSTALL.html)  
[《Elixir Korea》](http://elixir-kr.github.io/2015/08/23/section_3.html)