---
title:                "Fish Shell: 문자열을 소문자로 변환하기"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 대해 왜 이야기하려고 하는지 궁금하지 않으신가요? 리눅스 사용자분들은 당연히 터미널 환경에서 작업을 할 때 다양한 문자열을 다루게 됩니다. 때로는 대문자로 이루어진 문자열을 소문자로 변환해야 할 때가 있는데, 이를 위해서 어떤 방법이 있는지 알아보도록 하겠습니다.

## 어떻게
Fish Shell을 사용하면 간단하게 문자열을 소문자로 변환할 수 있습니다. 우선 다음의 예시 코드를 함께 보시죠.

```Fish Shell
set my_string "HELLO WORLD"
echo $my_string | string tolower
```

원래 문자열은 모두 대문자로 이루어져 있지만, `string tolower` 명령어를 사용하면 바로 소문자로 변환됩니다. 결과는 `hello world`라는 문자열이 출력됩니다. 쉬운 간단한 방법이지 않나요?

## 딥 다이브
그렇다면 더 깊이 들어가보겠습니다. Fish Shell에서 문자열을 소문자로 변환하는데 사용되는 명령어는 `string tolower`뿐만 아니라 `string totitle`과 `string toupper` 등도 있습니다. 또한 특정한 옵션을 추가하여 변환할 수도 있습니다. 이와 관련된 자세한 정보는 공식 문서를 참고해주세요. 더 나아가서, 편리한 문자열 관련 함수들을 더 많이 알아보시면 유용하게 활용할 수 있습니다.

## 더 알아보기
자세한 내용을 더 알고 싶으시다면 아래의 링크들을 참고해주세요.

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell 유용한 기능들](https://github.com/jorgebucaran/awesome-fish)
- [Fish Shell 사용법 및 팁](https://dev.to/codefather1/fish-shell-tips-and-tricks-295j)

## 관련 링크
- [Fish Shell로 문자열 자르기](https://fishshell.com/docs/current/cmds/cut.html)
- [Shell Script for Loops 사용하기](https://ryanstutorials.net/bash-scripting-tutorial/bash-loops.php)
- [리눅스 명령어 치트시트 정보 참고하기](https://www.thegeekstuff.com/linux-commands-cheat-sheet/)