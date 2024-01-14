---
title:                "Fish Shell: 문자열 연결하기"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 참여하는 이유는 매우 간단합니다. 이것은 작업하고 있는 언어와 도구에 따라 다양하게 불리지만, 큰 의미는 그래서 당신이 하고 싶은 일을 할 수 있다는 것입니다.

## 방법

```Fish Shell```은 문자열이 더해질 때 어떻게 동작하는지에 대해 매우 명확하고 직관적인 방식을 제공합니다. 아래의 예제를 살펴보세요:

\`\`\`Fish Shell
set greeting "안녕하세요 " 
set name "John Doe"
echo $greeting$name
\`\`\`

결과:

> 안녕하세요 John Doe

위 코드에서, 문자열을 더하는 방법은 "$"기호와 변수 이름으로 결합된다는 것을 알 수 있습니다. 이는 매우 간단하고 직관적인 방법입니다.

## 더 깊이 들어가보기

```Fish Shell```에서 문자열을 결합할 때, 명령어를 사용하면 좀 더 유연한 방법을 사용할 수 있습니다. 아래의 예제를 살펴보세요:

\`\`\`Fish Shell
set text "Hello, World!"
strinsert text 6 "awesome "
echo $text
\`\`\`

결과:

> Hello, awesome World!

위 코드에서, "strinsert" 명령어는 우리가 지정한 위치에 문자열을 삽입하는 것을 허용합니다. 이는 文자열을 결합할 때 논리적이고 강력한 방법을 제공하여 더 많은 기능을 추가할 수 있게 도와줍니다.

## 더 알아보기

```Fish Shell```에서 문자열을 더하는 방법에 대해 더 알아보고 싶다면, 이 링크들을 참고해보세요:

- https://fishshell.com/docs/current/index.html
- https://github.com/fish-shell/fish-shell
- https://stackoverflow.com/questions/tagged/fish

## See Also

- [Fish Shell 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 페이지](https://github.com/fish-shell/fish-shell)
- [Stack Overflow - Fish Shell 태그](https://stackoverflow.com/questions/tagged/fish)