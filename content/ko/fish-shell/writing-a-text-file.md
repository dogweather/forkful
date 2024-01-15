---
title:                "텍스트 파일 작성하기"
html_title:           "Fish Shell: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

어찌하면 텍스트 파일을 쓸 수 있을까요? 보통 이 작업은 왜 필요한 걸까요? 줄여 말하자면, 우리는 텍스트 파일을 만들어 여러 설정과 스크립트들을 저장하고 사용하기 위해서입니다. 예를 들어, 우리가 다시 시스템 설정을 해야 할 때마다 똑같은 명령어들을 반복하는 대신, 이전에 작성해둔 텍스트 파일을 사용하면 훨씬 더 빠르고 간단하게 설정을 할 수 있습니다.

## 어떻게

먼저, Fish 쉘을 사용하는 방법을 간단히 설명해보겠습니다. Fish 쉘은 다른 Unix 쉘들과 비교할 때 더 간단하고 직관적인 문법을 사용합니다. 그리고 이 쉘에는 자동완성 기능이 내장되어 있어 사용자가 명령어를 입력할 때 더 빠르고 효율적으로 작업할 수 있습니다.

```Fish Shell
# 텍스트 파일을 만드는 명령어
echo "여러분의 메시지" > 파일이름.txt

# 파일 내용을 추가하는 명령어
echo "추가할 내용" >> 파일이름.txt

# 파일을 읽는 명령어
cat 파일이름.txt
```

위의 예시에서 ```echo```는 해당 문자열을 출력하는 명령어이고, ```>```는 해당 내용을 지정한 파일에 쓰는 역할을 합니다. ```>>```는 파일 내용을 추가하는 명령어입니다. 마지막으로, ```cat```은 지정한 파일의 내용을 출력하는 명령어입니다.

## 깊게 파고들기

텍스트 파일을 다양한 방법으로 사용할 수 있습니다. 예를 들어, 스크립트 파일을 만들어 여러 명령어를 한 번에 실행시킬 수도 있습니다. 또는 Fish의 설정 파일인 ```config.fish```를 편집하여 자동 완성 기능이나 색상 설정 등 여러 가지 설정을 추가할 수 있습니다.

## 참고하기

- Fish 쉘 공식 홈페이지: https://fishshell.com/
- 코드 아카데미의 Fish 쉘 강좌: https://www.codecademy.com/learn/learn-the-command-line/modules/fish-shell
- Vi와 Emacs 등 텍스트 편집기 사용법은 검색을 통해 쉽게 찾을 수 있습니다.