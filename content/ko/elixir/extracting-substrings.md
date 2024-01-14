---
title:    "Elixir: 부분 문자열 추출"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 왜

## 왜 부분 문자열을 추출하나요?

Elixir는 강력한 언어로서 다양한 문자열 처리 기능을 제공합니다. 부분 문자열 추출은 그 중 하나로, 데이터 가공 및 처리에 있어서 필수적인 기능입니다.

# 어떻게

## 부분 문자열 추출하기

부분 문자열을 추출하는 방법은 다양합니다. 가장 일반적인 방법은 `String.slice/3` 함수를 사용하는 것입니다. 아래는 이 함수를 사용해 부분 문자열을 추출하는 예제 코드와 해당 코드의 출력 결과입니다.

```Elixir
string = "안녕하세요? 반가워요!"
String.slice(string, 2..4)
```

출력 결과:

```Elixir
"녕하"
```

위 코드에서 `String.slice/3` 함수는 첫 번째 인자로 추출할 문자열을, 두 번째 인자로 추출할 부분의 시작과 끝 인덱스를, 세 번째 인자로는 추출될 문자열의 옵션을 받습니다.

또 다른 방법은 정규식을 사용하는 것입니다. 정규식은 순수한 문자열 추출보다 더 복잡한 추출 작업을 수행하기에 적합할 수 있습니다.

# 심층 탐구

## 부분 문자열 추출에 대해 더 알아보기

부분 문자열 추출은 문자열 처리에 있어서 중요한 기능이지만, 주의해야 할 사항들도 있습니다. 예를 들어, 첫 번째 인덱스가 0이 아닌 경우 예상치 못한 결과를 얻을 수 있습니다. 또한 인덱스가 잘못 지정된 경우에도 역시 예상치 못한 결과를 얻을 수 있습니다.

따라서 부분 문자열 추출을 할 때는 인덱스를 확인하고 오류를 방지하는 것이 중요합니다.

# 참고

## 관련 링크들

- [Elixir 공식 문서 - String 모듈](https://hexdocs.pm/elixir/String.html)
- [Elixir 공식 문서 - 정규식](https://hexdocs.pm/elixir/Regex.html)
- [10분 Elixir - 문자열 조작하기](https://blog.eom.dev/ko/dev-tips-how-to-manipulate-strings-in-elixir-in-10-minutes/)