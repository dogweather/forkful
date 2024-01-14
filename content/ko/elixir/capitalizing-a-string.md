---
title:                "Elixir: 문자열 대문자로 바꾸기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 만들고 싶은 이유는 문자열을 더 읽기 쉽게 만들기 위해서입니다.

## 만드는 법

우선 문자열에 있는 대문자를 소문자로 바꾸고 싶은 경우, Elixir 내장 함수 중 `String.downcase/1` 함수를 이용할 수 있습니다. 코드는 다음과 같습니다.

```Elixir
string = "hELlo wOrLd!"
result = String.downcase(string)
```
출력 결과는 `"hello world!"`입니다.

이와 반대로, 문자열의 첫 글자를 대문자로 바꾸기 위해서는 `String.capitalize/1` 함수를 사용할 수 있습니다. 다음 코드는 문자열의 첫 글자만 대문자로 바꾸는 예시입니다.

```Elixir
string = "hello world!"
result = String.capitalize(string)
```
출력 결과는 `"Hello world!"`입니다.

## 깊게 파헤치기

하지만 만약 문자열에 있는 모든 단어의 첫 글자를 대문자로 바꾸기를 원한다면 어떻게 해야 할까요? 이때는 `String.split/2` 함수를 이용하여 공백을 기준으로 문자열을 나눈 뒤 각 단어의 첫 글자를 대문자로 만든 뒤 다시 합치면 됩니다. 아래 코드는 이 과정을 나타낸 것입니다.

```Elixir
string = "hello world!"
words = String.split(string, " ")
result = for word <- words, do: String.capitalize(word)
result = Enum.join(result, " ")
```
출력 결과는 `"Hello World!"`입니다.

## 더 읽어보기

- [Elixir 공식 문서](https://elixir-lang.org/docs.html)
- [Elixir 공식 GitHub 저장소](https://github.com/elixir-lang/elixir)