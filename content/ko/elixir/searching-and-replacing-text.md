---
title:    "Elixir: 텍스트 검색 및 대체"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 왜
 텍스트를 검색하고 바꾸는 것에 참여해야하는 이유는 무엇일까요? 우리는 일상에서 많은 시간을 텍스트를 읽고 쓰는데에 할애합니다. 때로는 비슷한 내용을 다른 단어로 표현하거나, 잘못된 단어를 수정해야할 때가 있을 수 있습니다. 이러한 경우 우리는 대체하여 텍스트를 쉽게 수정할 수 있는 방법이 필요합니다. 이를 위해 우리는 Elixir를 사용할 수 있습니다.

## 어떻게
"```Elixir
# 문자열 "안녕하세요"에서 "안녕"을 "하이"로 바꾸는 예제
str = "안녕하세요"
str = String.replace(str, "안녕", "하이")
IO.puts(str) # "하이하세요"
"```

우리는 String 모듈의 replace 함수를 사용하여 원하는 문자열을 다른 문자열로 대체할 수 있습니다. 이 함수는 첫번째 인수로 대상 문자열, 두번째 인수로 원하는 문자열, 세번째 인수로 대체할 문자열을 받습니다. 위 코드 예제에서는 "안녕"을 "하이"로 대체하였습니다.

"```Elixir
# 가장 인기있는 인사말을 다양한 언어로 바꾸는 예제
i18n_map = %{"안녕" => "하이", "こんにちは" => "ハロー", "Hello" => "Hi"}
str = "안녕하세요, 반가워요"
str = String.replace(str, "안녕", Map.get(i18n_map, "안녕"))
IO.puts(str) # "하이하세요, 반가워요"
"```

위 예제에서는 인사말을 다국어로 저장하고 이를 바탕으로 원하는 언어로 대체하는 방법을 보여줍니다.

## 심층 분석
문자열 대체는 간단해 보이지만 내부적으로는 어떻게 작동할까요? 우리는 Elixir의 String 모듈 내부 코드를 살펴보며 깊이 파고들어 볼 수 있습니다. 또한 패턴 매칭과 정규표현식을 활용하여 더 다양한 대체 방법을 알아볼 수 있습니다.

## 더 알아보기
- [Elixir 문서 : String 모듈](https://hexdocs.pm/elixir/String.html)
- [Elixir Forum : 문자열 대체 방법 공유](https://elixirforum.com/t/can-you-explain-how-string-replace-working-behind-the-scene/24231)
- [Alchemist's Toolbox : 문자열 대체 기술 논의](https://blog.thealchemistdeveloper.com/elixir/pattern-matching-string-replace-example.html)

# 참고
보다 복잡한 문자열 대체 방법을 알고 싶다면 위 링크들을 참고하시기 바랍니다. 또한 Elixir 외에도 다른 프로그래밍 언어에서도 문자열 대체에 관한 내용을 찾아보실 수 있습니다.