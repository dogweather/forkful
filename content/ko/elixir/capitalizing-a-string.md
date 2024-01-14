---
title:    "Elixir: 문자열 대문자화"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

왜 누군가가 문자열을 대문자로 표기하는 것에 관심을 가져야 할까요? Elixir는 매우 강력한 프로그래밍 언어이며, 문자열을 다양한 방법으로 다룰 수 있기 때문입니다.

## 어떻게

우리는 다음의 코드를 사용하여 문자열을 대문자로 변환 할 수 있습니다:

```Elixir
string = "hello world"
String.upcase(string)
```

그 결과는 다음과 같을 것입니다:

```Elixir
"HELLO WORLD"
```

또는 우리는 함수를 사용하여 직접 문자열을 대문자로 변환 할 수도 있습니다:

```Elixir
def capitalize_string(string) do
  String.upcase(string)
end
```

## 심도있는 분석

문자열을 대문자로 표기하는 깊이있는 지식을 갖고 싶다면, 우리는 문자열 처리에 대해 다른 방법을 조사해 볼 수 있습니다. 문자열을 분할하고 요소를 변환하기 위해 Elixir의 문자열 함수인 `String.split/3`와 `Enum.map/2`를 사용할 수 있습니다:

```Elixir
string = "apple, banana, orange"
String.split(string, ", ") 
|> Enum.map(fn word -> String.capitalize(word) end)
|> Enum.join(", ")
```

결과는 다음과 같을 것입니다:

```Elixir
"Apple, Banana, Orange"
```

따라서 우리는 더 복잡한 문자열 작업을 수행하는 데 필요한 도구를 익혀 나갈 수 있습니다.

## 더 알아보기

- [Elixir 공식 문서](https://hexdocs.pm/elixir/String.html)
- [Elixir 공식 다수의 문자열 처리 방법](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#strings)
- [Elixir 문자열 함수 사용 예제](https://www.tutorialspoint.com/elixir/elixir_string_upcase.htm)

## 참고

- 크래킹 코드에 대한 소스 코드 및 연습 문제를 포함하는 [Elixir 챌린지](https://github.com/elixirko/elixir-crash-course)
- Elixir를 연습하고 기술을 개발하기 위한 [Awesome Elixir](https://awesomeopensource.com/project/h4cc/awesome-elixir)