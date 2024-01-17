---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "Fish Shell: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
패턴과 일치하는 문자를 삭제하는 것이 무엇인지 및 프로그래머들이 왜 이를 수행하는지에 대해 두 개 또는 세 개의 문장으로 설명합니다.

이 기능은 한 번에 여러 파일에서 일부 내용을 변경하고자 할 때 유용합니다. 예를 들어, 모든 파일에서 특정 단어를 바꾸거나, 특정 패턴의 코드를 모두 삭제하고 싶을 때 사용할 수 있습니다. 

## 사용 방법:
아래 코드 블록으로 표시된 코딩 예제와 출력 결과를 이용하여 설명합니다. 

```Fish Shell 
grep -rl 패턴 디렉토리 | xargs sed -i "s/패턴//g"
```

코드 블록의 첫 번째 줄은, 디렉토리 내에서 패턴과 일치하는 파일을 찾아주는 ```grep``` 명령어입니다. 두 번째 줄에서는 ```xargs```를 이용하여 해당 파일들을 인자로 받아와서 ```sed``` 명령어를 이용하여 패턴과 일치하는 부분을 모두 삭제합니다. 

## 더 알아보기:
삭제 패턴 매칭 기능은 과거에는 슈퍼컴퓨터들을 이용하여 대규모 병렬 처리를 통해 수행되었지만, 현재는 일반 컴퓨터에서도 매우 빠르게 처리할 수 있습니다. 

알터너티브로는, 패턴을 수정하는 대신 해당 내용을 바꾸는 방법도 있습니다. 하지만 이를 사용하는 경우, 삭제보다는 변경에 초점을 맞추게되고, 그에 따라 패턴의 선택이 올바르게 되는 것이 중요해지게 됩니다. 

삭제 패턴 매칭 기능은 일반적으로 ```sed```나 ```awk``` 같은 명령어를 이용하여 구현되는 경우가 많습니다. 하지만 Fish Shell에 내장되어 있어서, 별도의 패키지나 다른 프로그램 없이 바로 사용할 수 있습니다.

## 참고 자료:
관련된 자료들의 링크를 첨부합니다.
- [Fish Shell 문서](https://fishshell.com/docs/current/)
- [sed 명령어 문서](https://www.gnu.org/software/sed/manual/sed.html)
- [awk 명령어 문서](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Linux/Unix 명령어 간단 안내서](https://ryanstutorials.net/linuxtutorial/piping.php)
- [grep 명령어 예제](https://www.cyberciti.biz/faq/howto-use-gpufiles-which-need.php)