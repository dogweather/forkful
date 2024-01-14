---
title:                "Elixir: 문자열 연결하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열 연결(concatenation)을 하는 이유는 간단합니다 - 여러분은 하나의 큰 문자열을 만드는데 필요한 작은 부품들을 결합시킬 수 있기 때문입니다.

## 하는 법

```Elixir
# 문자열 연결 예제
str1 = "안녕"
str2 = "하세요"
concatenated = str1 <> str2

# 샘플 출력
IO.puts concatenated # 출력 결과: "안녕하세요"
```

문자열 연결은 Elixir에서 매우 쉽게 할 수 있습니다. `<>` 연산자를 사용하여 문자열을 결합하면 됩니다. 또한 변수에 할당하지 않고도 직접 출력할 수도 있습니다.

```Elixir
# 변수를 사용하지 않고 직접 출력하는 예제
IO.puts "안녕" <> "하세요" # 출력 결과: "안녕하세요"
```

## 깊게 파헤치기

문자열 연결은 대부분의 프로그래밍 언어에서 매우 일반적으로 사용되는 기능입니다. 그러나 Elixir에서는 이 기능을 더욱 강력하고 유연하게 활용할 수 있도록 다양한 옵션을 제공합니다.

### 변수를 사용하지 않고 문자열 연결하기

실제로 변수를 사용하지 않고도 문자열을 바로 연결하여 출력할 수 있습니다. 이는 매우 간편하고 직관적이기 때문에 적극적으로 활용하는 것을 권장합니다.

### 여러 개의 문자열 연결하기

`<>` 연산자를 여러 번 사용하여 여러 개의 문자열을 연결할 수 있습니다.

```Elixir
# 여러 개의 문자열 연결 예제
str1 = "빨강"
str2 = "주황"
str3 = "노랑"
concatenated = str1 <> str2 <> str3

# 샘플 출력
IO.puts concatenated # 출력 결과: "빨강주황노랑"
```

### 다른 데이터 유형과 문자열 연결하기

Elixir에서는 문자열과 다른 데이터 유형을 자동으로 문자열로 변환하여 연결할 수 있습니다. 이는 매우 강력한 기능입니다. 예를 들어, 정수를 문자열과 함께 연결할 수 있습니다.

```Elixir
# 다른 데이터와 문자열 연결 예제
str = "나는"
num = 25
concatenated = str <> num

# 샘플 출력
IO.puts concatenated # 출력 결과: "나는25"
```

또는 리스트 안에 있는 요소들을 모두 연결할 수도 있습니다.

```Elixir
# 리스트 안에서 문자열 연결하기 예제
str1 = "안녕"
str2 = "하세요"
lst = [str1, str2]
concatenated = Enum.join(lst, "-")

# 샘플 출력
IO.puts concatenated # 출력 결과: "안녕-하세요"
```

## 더 알아보기

문자열 연결 외에도 Elixir에서 제공하는 다양한 문자열 관련 기능들이 있습니다. 다음 링크들을 참고하여 더 많은 정보를 얻어보세요.

[문자열과 리스트 다루기](https://elixir-lang.org/getting-started/basic-types.html#strings-and-char-lists)

[문자열 포맷팅](https://hexdocs.pm/elixir/String.html#module-formatting-strings)

## 관련 링크

[공식 Elixir 문서](https://elixir-lang.org/)

[Elixir School - 온라인 Elixir 학습 리소스](https://elixirschool.com/ko/)

[Elixir TV - Elixir 관련 비디오 강의](https://www.elixirtv.com/)