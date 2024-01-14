---
title:                "Elixir: 부분 문자열 추출하기"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 substring을 추출해야 할까요?

substring 추출은 실제로 매우 유용한 프로그래밍 기능입니다. substring을 추출하는 것은 문자열을 조작하는 데 도움이 되며, 일련의 문자열 중 필요한 부분만 선택할 수 있게 해줍니다.

## 방법: substring 추출하는 법

자바스크립트에서 substring을 추출하는 방법은 간단합니다. 코드 블록 내에서 문자열을 지정하여 `substring/3` 함수를 사용할 수 있습니다.

```Elixir
str = "안녕하세요, 제 이름은 Elixir입니다."
substring(str, 9, 11)
```
출력은 `"제 이름은"`이 될 것입니다.

## 깊이 탐구: substring 추출에 대해 더 알아보기

substring은 문자열 뿐만 아니라 리스트에서도 작업할 수 있습니다. 따라서 Elixir에서는 문자열을 다루는 데 매우 유용한 기능입니다. 문자열의 한 부분만 선택하여 다른 작업에 사용할 수 있게 되므로 문자열을 더 효율적으로 관리할 수 있습니다.

함수가 간단하게 작성되어 있기 때문에 Elixir에서 substring을 사용하는 것은 매우 쉽습니다. 또한, string manipulation을 위한 다양한 함수들과 함께 제공되기 때문에 substring 함수를 함께 사용하면 좋은 결과를 얻을 수 있습니다.

## 또 다른 참고 자료

- [Elixir String API](https://hexdocs.pm/elixir/String.html)
- [Learn Elixir in Y minutes](https://learnxinyminutes.com/docs/elixir/)
- [Mastering Elixir Strings](https://medium.com/blackode/matering-elixir-strings-5f5e8cc5a5f8)

# 함께 보기

[안녕하세요, 제 이름은 Elixir입니다.](https://helloelixir.com/)