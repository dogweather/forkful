---
title:    "Elixir: 정규 표현식을 사용하기"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜
정규 표현식을 사용하는 이유는 무엇일까요? 정규 표현식은 문자열 내에서 패턴을 검색하거나 대체하는 등 다양한 작업을 할 수 있기 때문에 효율적이고 빠른 방법입니다.

## 하우 투
아래의 예제 코드와 함께 정규 표현식을 사용하는 방법을 살펴봅시다.

```elixir
# 문자열 내에서 패턴 검색
Regex.scan(~r/world/, "Hello world") #=> ["world"]

# 패턴 대체
Regex.replace(~r/world/, "Hello world", "Hi") #=> "Hello Hi"

# 패턴 매칭 외에도 여러 유용한 메소드를 제공합니다.
Regex.split(~r/,/, "1,2,3,4") #=> ["1", "2", "3", "4"]
```

## 딥 다이브
정규 표현식은 강력한 도구이지만 파싱 규칙을 작성하는 것은 어려운 작업일 수 있습니다. 그래서 Elixir에서는 정규 표현식 대신 [구문 분석기](https://hexdocs.pm/elixir/master/task.html)를 사용하는 것도 고려해볼 만한 방법입니다.

## 더 알아보기
정규 표현식에 대해 더 자세히 알아보려면 아래 링크들을 참고해보세요.

- [Elixir 정규 표현식 문서](https://hexdocs.pm/elixir/Regex.html)
- [정규 표현식을 사용한 문자열 검증](https://www.regular-expressions.info/elixir.html)
- [정규 표현식 연습 사이트](https://regexr.com/)