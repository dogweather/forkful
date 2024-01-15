---
title:                "표준 오류에 쓰는 방법"
html_title:           "Fish Shell: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

오늘은 `Fish Shell`의 최신 버전을 소개해드리려고 합니다. 아마 대부분의 분들이 `Fish Shell`을 이미 사용해보셨을 것으로 생각됩니다. 이번 글에서는 표준 오류에 쓰기 위해 코딩을 하는 이유에 대해 알아보도록 하겠습니다.

코딩을 하는 이유는 여러 가지가 있지만, 대부분의 경우에는 표준 오류에 대한 정보를 제대로 알기 위해서 입니다. 예를 들어, 프로그램이 정상적으로 작동하지 않을 때 오류 메시지를 통해 문제를 파악하는 경우에는 표준 오류에 써져있는 내용을 확인하는 것이 중요합니다.

## 어떻게 하나요?

여러분이 `Fish Shell`을 사용하면서 많은 오류 메시지를 본 적이 있으실겁니다. 이러한 오류 메시지는 표준 오류에 쓰여지는 내용들입니다. 만약 여러분이 그냥 이 메시지를 무시하고 지나가신다면, 오류를 해결하는데 큰 도움이 되지 않을 수 있습니다.

따라서 오류 메시지를 이해하고 해당 오류를 해결하기 위해서는 표준 오류에 쓰여져있는 내용들을 읽고 이해하는 것이 중요합니다. 이를 위해 `Fish Shell`에서는 표준 오류에 써지는 내용들을 보다 쉽게 확인할 수 있도록 다양한 방법을 제공하고 있습니다.

```Fish Shell
echo "This is an error message" >&2
```

위와 같이 코드를 작성하면 "This is an error message"라는 내용이 표준 오류에 쓰여지게 됩니다. 여기서 우리는 "&"와 "2"라는 특수기호를 사용하고 있는데요, 이는 표준 오류에 쓰이는 내용을 가리키는 것입니다. 그리고 `echo`라는 명령어를 사용해서 우리가 원하는 내용을 출력할 수 있도록 해줍니다.

## 깊게 들어가보기

여러분은 아마도 `Fish Shell`에서 이러한 코드를 이해하고 작성할 수 있을 것입니다. 하지만 더 깊게 들어가서 표준 오류에 대해 더 자세히 알아보도록 하겠습니다.

표준 오류는 일반적으로 프로그램이 실행될 때 발생하는 오류를 표시합니다. 따라서 프로그래밍을 할 때, 이를 적절히 처리하는 것이 매우 중요합니다. 만약 오류 메시지를 무시하고 넘어가게 된다면, 프로그램이 원하는 대로 작동하지 않을 수 있습니다.

따라서 표준 오류에 쓰여진 내용을 잘 이해하고 적절하게 처리하는 것이 프로그래밍의 기본이라고 할 수 있습니다.

## 관련 글

- [Fish Shell 공식 홈페이지](https://fishshell.com)
- [Fish Shell 소개 및 사용 방법 (영문)](https://www.freecodecamp.org/news/an-introduction-to-the-fish-shell-prompt-and-basics-817f211328c8/)
- [Fish Shell 특징 및 사용 예제 (영문)](https://www.howtogeek.com/285574/fish-is-a-smart-and-user-friendly-command-line-shell/)
- [Fish Shell 디렉토리 구조 및 기본 명령어 (영문)](https://ostechnix.com/f