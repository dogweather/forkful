---
title:    "Elixir: 패턴과 일치하는 문자 삭제하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

캐릭터 패턴과 일치하는 문자를 삭제하는 것에 대해 관심을 가져야 하는 가장 큰 이유는 데이터 정제입니다. 올바른 데이터를 사용해야만 정확한 결과를 얻을 수 있기 때문입니다. 또한 일치하는 문자를 삭제하는 것은 코드의 효율성을 높이고 보다 깔끔한 데이터를 유지하는 데 도움이 됩니다.

## 방법

우선, Elixir에서 문자열을 다루는 강력한 기능 중 하나인 정규식을 사용하는 방법을 알아보겠습니다. 우리는 `Regex.delete/2` 함수를 사용해 일치하는 패턴을 삭제할 수 있습니다. 먼저 우리가 삭제하려는 패턴을 정의하고, 이를 `Regex.compile/1` 함수에 전달하여 정규식을 생성합니다. 그리고 `Regex.replace/4` 함수를 사용해 일치하는 문자를 삭제합니다.

**예시 코드:**

```Elixir
pattern = ~r/꽁|새*이/
regex = Regex.compile(pattern)
string = "김꽁꽁과 이새해는 친구입니다."

Regex.replace(string, regex, "")
# 출력 결과: 김과 는 친구입니다.
```

또 다른 방법은 `String.replace/3` 함수를 사용하는 것입니다. 이 함수는 문자열 내에서 정해진 패턴을 가진 문자를 대체하는 데 사용됩니다.

**예시 코드:**

```Elixir
pattern = ~r/안녕/
string = "안녕하세요, 반가워요!"

String.replace(string, pattern, "")
# 출력 결과: 하세요, 반가워요!
```

## 깊이 파고들기

패턴을 정의하는 방법에 대해 더 자세히 살펴보겠습니다. 우선, 정규식의 기본 구문은 `~r//` 형태입니다. 그 안에 우리가 찾고자 하는 패턴을 작성하면 됩니다. `~r`을 `~r/`로 대체하는 것은 선택 사항입니다.

패턴에는 다양한 특수 문자를 사용할 수 있습니다. 예를 들어 `|`는 여러 패턴 중 하나에 일치하는 경우를 포함합니다. 위의 예시 코드에서는 "꽁" 또는 "새" 다음에 "이"라는 문자가 오는 부분을 삭제했습니다.

그리고 `*`는 앞의 문자가 0번 이상 나타나야 한다는 것을 의미합니다. `+`는 앞의 문자가 1번 이상 나타나야 한다는 것을 의미하며, `?`는 앞의 문자가 0번 또는 1번 나타나야 한다는 것을 의미합니다.

더 많은 정규식 구문에 대해 알고 싶다면 [Elixir 공식 문서](https://hexdocs.pm/elixir/Regex.html)를 참고하시기 바랍니다.

## 더 알아보기

지금까지 캐릭터 패턴을 삭제하는 방법에 대해 알아보았습니다. 하지만 정규식은 문자열을 처리하는 데 더 다양한 기능을 제공합니다. 다른 유용한 정규식 함수를 알고 싶다면 [Elixir 공식 문서](https://hexdocs.pm/elixir/Regex.html)를 참고하시기 바랍니다.

## 함께 보기

- [Elixir에서의 정규식 사용 방법(Queue in Elixir) - 번역 중](https://elixirstatus.com/p/oq-y3bk-regular-expressions-in-elixir-queues-in-elixir)
- [Elixir에서 정규식 활