---
title:    "Elixir: 문자열 연결하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것이 왜 중요한지 궁금하지 않으신가요? 이것은 우리가 문자열 조작을 할 때 매우 유용합니다. 예를 들어, 사용자 이름 및 이메일 주소를 결합하여 메일링 리스트에 추가할 수 있습니다.

## 어떻게

"为何"

Elixir에서 문자열을 연결하려면 전달 된 문자열을 `<>` 연산자로 감싸주면 됩니다. 예를 들어:

```elixir
"안녕" <> "하세요"
```

위와 같은 코드를 실행하면 "안녕하세요" 라는 문자열이 반환됩니다.

이제 우리는 보다 복잡한 예제를 살펴보겠습니다. 우리가 아래와 같은 사용자 정보가 담긴 맵을 가지고 있다고 가정해봅시다:

```elixir
user = %{name: "신우정", email: "shinwoo@example.com"}
```

이제 우리는 이름과 이메일 주소를 결합하여 메일링 리스트에 추가하고 싶습니다. 우리는 이를 위해 다음과 같은 코드를 작성할 수 있습니다.

```elixir
user[:name] <> " - " <> user[:email]
```

실행 결과는 "신우정 - shinwoo@example.com" 가 됩니다.

## 깊게 들어가기

그렇다면 왜 우리는 문자열을 조작할 때 `<>` 연산자를 사용할까요? 이것은 Elixir에서 매우 효율적인 방법이기 때문입니다. Elixir는 불변성을 강조하며 문자열을 변경하는 대신 새로운 문자열을 반환합니다. 따라서 `<>` 연산자를 사용하면 새로운 문자열을 생성하지 않고 기존의 두 문자열을 조합할 수 있습니다.

## 또한 보기

이제 문자열 연결에 대해 기본적인 지식을 가지게 되었으니 다른 Elixir 문자열 조작 기술을 배워보세요. 아래는 참고할 수 있는 몇 가지 유용한 링크입니다.

- [Elixir 문자열 문서](https://hexdocs.pm/elixir/String.html)
- [Elixir 문자열 처리 팁](https://blog.appsignal.com/2019/03/26/elixir-alchemy-string-handling-tips.html)
- [Elixir Strings 패턴 매칭](https://blog.appsignal.com/2019/01/29/alchemist-s-camp-elixir-pattern-matching-strategies-and-string-manipulation-techniques.html)