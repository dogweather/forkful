---
title:    "Elixir: 문자열 연결하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

# 왜: 문자열을 연결하는 것의 이유

문자열을 연결하는 것은 프로그래밍에서 자주 사용되는 작업 중 하나입니다. 예를 들어, 사용자에게 메시지를 보내거나 데이터베이스에서 값을 가져와서 표시하는 등의 경우에 문자열을 연결하는 작업이 필요할 수 있습니다.

## 방법: 코드 예제와 출력 결과

```Elixir
# 문자열 연결 예제
string1 = "안녕하세요, "
string2 = "저는 "
string3 = "Elixir를 사랑합니다."

IO.puts string1 <> string2 <> string3

# 출력 결과: 안녕하세요, 저는 Elixir를 사랑합니다.
```

위 코드에서는 `<>` 연산자를 사용하여 문자열을 연결했습니다. 또한 여러 개의 문자열도 한 번에 연결할 수 있습니다.

```Elixir
# 여러 개의 문자열을 한 번에 연결하는 예제
string_list = ["저는", "Elixir를", "사랑합니다."]
IO.puts Enum.join(string_list, " ")

# 출력 결과: 저는 Elixir를 사랑합니다.
```

더 복잡한 문자열 연결에는 `String.interpolate/2` 함수를 사용할 수 있습니다.

```Elixir
# String.interpolate/2 함수 예제
name = "홍길동"
age = 30
IO.puts String.interpolate("저는 #{name}이고 나이는 #{age}살입니다.")

# 출력 결과: 저는 홍길동이고 나이는 30살입니다.
```

## 깊이 파고들기: 문자열 연결에 대한 더 깊은 정보

Elixir에서 문자열을 연결하는 가장 일반적인 방법은 `<>` 연산자를 사용하는 것입니다. 이는 내부적으로 `String.concat/1` 함수를 사용하여 문자열을 연결합니다. 이 함수는 입력으로 리스트를 받을 수도 있기 때문에 `Enum.join/2`와 같은 함수와도 함께 사용할 수 있습니다.

또한 문자열 연결은 시간 복잡도 측면에서 유의해야 할 부분입니다. `<>` 연산자는 매우 빠르게 실행되지만, 문자열 연결을 위해 매번 새로운 문자열을 생성하는 것은 비용이 많이 들게 되어 성능상의 이슈가 발생할 수 있습니다. 이를 방지하기 위해 `StringBuilder` 라이브러리를 사용하여 효율적인 문자열 연결을 할 수 있습니다.

# 또 보기

[Elixir 문자열 연결 공식 문서(en)](https://elixir-lang.org/getting-started/string-interpolation.html#string-concatenation)

[Elixir String 모듈 공식 문서(en)](https://hexdocs.pm/elixir/String.html)

[Elixir Enum 모듈 공식 문서(en)](https://hexdocs.pm/elixir/Enum.html#join/2)