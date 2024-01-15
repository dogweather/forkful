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

## 왜

어떤 경우에 문자열 패턴과 일치하는 문자를 삭제하는 것이 유용한지 알고 싶으신가요? 예를 들어서, 여러분이 파일 이름을 바꾸거나, 특정 문자를 교체하거나, 불필요한 공백을 제거하고 싶을 때 등 다양한 상황에서 문자 삭제 기능이 유용합니다.

## 어떻게

일치하는 문자를 삭제하는 기능은 매우 간단합니다. 우선 Fish Shell에서 `sed` 명령어를 사용하여 원하는 패턴에 해당하는 문자를 삭제해주세요. 예를 들면 다음과 같습니다.

```Fish Shell
sed "s/pattern//g" file.txt
```

위의 예시에서 `sed` 명령어를 사용하여 `file.txt` 파일에서 `pattern`에 해당하는 모든 문자를 삭제하였습니다. 이제 파일을 열어보면 해당 패턴에 해당하는 문자들이 모두 제거된 것을 확인할 수 있습니다.

## 딥 다이브

이제 좀 더 자세한 사항을 알아보겠습니다. `sed "s/pattern//g"` 명령어는 실제로 `sed` 프로그램 안에서 정규 표현식을 사용하고 있습니다. 따라서 사용하는 정규 표현식에 따라 매우 다양한 패턴을 삭제할 수 있습니다. 그리고 `g` 옵션은 해당 패턴을 찾은 모든 문자를 삭제하라는 것을 의미합니다. 만약 `g` 옵션을 빼고 실행한다면, 첫 번째로 발견한 패턴만 삭제하게 됩니다.

## 더 보기

- [Fish Shell 공식 홈페이지](https://fishshell.com/)
- [Fish Shell Github 레포지토리](https://github.com/fish-shell/fish-shell)
- [Fish Shell 사용법 영상 강좌](https://youtu.be/RCHPiZ6DgBI)