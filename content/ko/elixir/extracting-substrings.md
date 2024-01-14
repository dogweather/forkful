---
title:    "Elixir: 서브스트링 추출하기"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

자바스크립트에서는 많은 경우 문자열에서 특정 부분을 추출해야 할 때가 있습니다. 예를 들어, 이메일 주소에서 사용자 이름을 추출하거나, URL에서 도메인 이름을 추출할 수 있습니다. 이러한 작업을 위해 Elixir에서는 `String.slice/3` 함수를 사용할 수 있습니다.

## 방법

우리가 추출하고자 하는 문자열과 추출하고자 하는 부분의 시작과 끝 인덱스를 지정하는 것으로 간단하게 부분 문자열을 추출할 수 있습니다. 이를 위해 `String.slice/3` 함수를 사용하며, 첫 번째 인자는 추출하고자 하는 문자열, 두 번째 인자는 시작 인덱스, 세 번째 인자는 끝 인덱스입니다.

```elixir
full_name = "박지성"
surname = String.slice(full_name, 0, 1) # "박"
```

위의 예시에서 `full_name` 변수에는 "박지성" 문자열이 저장되어 있으며, `String.slice/3` 함수를 사용해 첫 번째 글자만 추출하여 `surname` 변수에 저장하였습니다.

추출하고자 하는 부분이 여러 글자인 경우, 시작과 끝 인덱스를 적절히 조절해준 후 `String.slice/3` 함수를 사용할 수 있습니다.

```elixir
full_name = "박지성"
last_name = String.slice(full_name, 1, 3) # "지성"
```

위의 예시에서는 `String.slice/3` 함수를 사용해 두 번째 글자부터 세 번째 글자까지 추출하여 `last_name` 변수에 저장하였습니다.

## 딥 다이브

이 밖에도 `String.slice/3` 함수를 사용할 때는 다양한 옵션을 제공할 수 있습니다. 예를 들어, 인덱스를 음수로 지정하면 문자열의 뒤에서부터 추출할 수도 있습니다.

```elixir
full_name = "박지성"
last_letter = String.slice(full_name, -1, -2) # "공"
```

위의 예시에서는 `last_letter` 변수에 "공"이 저장되어 있습니다.

또한, 추출하고자 하는 부분이 문자열을 벗어날 경우 `String.slice/3` 함수는 자동으로 시작 인덱스를 최대값 또는 끝 인덱스를 최소값으로 설정해줍니다. 예를 들어, 인덱스가 -1과 같거나 클 경우 시작 인덱스는 0, 끝 인덱스는 문자열의 길이가 됩니다.

```elixir
full_name = "Harry Kane"
first_name = String.slice(full_name, 0, 5) # "Harry"
last_name = String.slice(full_name, 6, 20) # "Kane"
```

위의 예시에서는 `first_name` 변수에는 "Harry"가, `last_name` 변수에는 "Kane"이 저장되었습니다.

## 참고 자료

- Elixir 공식 문서: https://hexdocs.pm/elixir/String.html#slice/3
- 분당 Elixir 모임: https://www.facebook.com/bundangelixir/
- Elixir 스터디 그룹: https://www.facebook.com/groups/elixirstudy/