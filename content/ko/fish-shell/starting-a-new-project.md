---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Fish Shell 새 프로젝트 시작하기

## 무엇 그리고 왜?

새 프로젝트를 시작하는 것은 코드를 작성하기 위한 기반을 구성하는 과정입니다. 이는 개발자들이 목표를 달성하고, 구조적이며 관리하기 쉬운 코드를 유지하려는 의도에서 비롯된다.

## 어떻게 해야 할까?

Fish Shell에서 새 프로젝트 시작은 아래와 같습니다.

```Fish Shell
cd ~
mkdir new_project && cd new_project
touch main.fish
```

위의 코드는 홈 디렉토리로 이동한 후 `new_project`라는 이름의 새 디렉토리를 만들고 그 안에 `main.fish`라는 새 파일을 만들어줍니다.

## 깊이 들여다보기

Fish Shell은 2005년에 출시되어 편의성을 위해 설계된 사용자 친화적인 command line shell입니다. 대안으로는 Bash, Zsh, Tcsh 등이 있습니다. Fish Shell에서는 tab 키를 이용한 auto suggestion, 매우 강력한 syntax highlighting 및 내장된 웹 기반 설정 등 이용자 편의를 위해 다양한 기능을 가지고 있습니다.

## 참고 자료

[Fish Shell 공식 홈페이지](https://fishshell.com/)
[Fish Shell GitHub](https://github.com/fish-shell/fish-shell)