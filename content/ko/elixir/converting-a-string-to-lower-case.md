---
title:    "Elixir: 문자열을 소문자로 바꾸기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것의 중요성은 Elixir 프로그래밍에서 문자열을 다루는 데에 있습니다. 이 기능을 사용하면 문자열을 비교하거나 검색할 때 대소문자를 무시할 수 있으며 결과물을 일관되게 만들 수 있습니다.

## 하우 투

```
Elixir defp downcase(string) do
  String.downcase(string)
end

IO.puts downcase("Hello, World!")
```

```
# Output: "hello, world!"
```

위의 코드에서는  `String.downcase/1` 함수를 사용하여 문자열을 소문자로 변환하는 함수를 만듭니다. 이를 `downcase/1` 함수를 사용하여 출력하면 입력한 문자열이 소문자로 변환되어 출력됩니다.

## 딥 다이브

이제 문자열을 소문자로 변환하는 방법에 대해 더 자세히 알아보도록 하겠습니다. Elixir에서는 문자열을 처리할 때 모두 `String` 모듈에서 제공하는 함수들을 사용합니다. 즉, `String.downcase/1` 함수 또한 `String` 모듈에서 제공하는 함수 중 하나입니다.

`String.downcase/1` 함수는 먼저 입력받은 문자열의 각 문자를 하나씩 처리합니다. 이때 문자의 유니코드 값을 확인하여 대문자인 경우 소문자로 변환시킨 후 결과물을 반환합니다. 그렇기 때문에 이 함수를 사용하면 모든 언어의 문자를 소문자로 변환할 수 있습니다.

## 참고 자료

- [Elixir String 모듈 문서](https://hexdocs.pm/elixir/String.html)