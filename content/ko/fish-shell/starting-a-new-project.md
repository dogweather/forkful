---
title:                "새로운 프로젝트 시작하기"
html_title:           "Fish Shell: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜
새 프로젝트를 시작하는 것이 중요한 이유는 무엇일까요? 어떻게 하면 현재의 Fish Shell을 이용하여 새로운 프로젝트를 시작할 수 있을까요? 이 글에서는 더 깊게 살펴보겠습니다.

## 어떻게 해야 할까요?
Fish Shell에서 새 프로젝트를 시작하기 위해서는 다음과 같은 단계를 따라야 합니다.

1. 우선 새로운 디렉토리를 만듭니다.
    ```fish
    mkdir new_project
    ```
2. 해당 디렉토리로 이동합니다.
    ```fish
    cd new_project
    ```
3. 프로젝트 이름을 설정합니다.
    ```fish
    set project_name my_project
    ```
4. 이제 해당 디렉토리에 Fish 프로젝트 초기화 파일을 생성합니다.
    ```fish
    fish_project init
    ```
5. 생성된 파일은 `.fish_project`입니다. 해당 파일을 열어서 프로젝트 설정값을 수정할 수 있습니다.
6. 모든 설정이 끝났다면, 프로젝트를 실행합니다.
    ```fish
    fish_project start
    ```
7. 원하는대로 프로젝트를 수정하고 코드를 작성해보세요!

## 깊게 파고들기
새 프로젝트를 시작하는 것은 매우 중요합니다. 왜냐하면 새로운 기능을 추가하거나 기존 기능을 개선하는 등의 작업을 할 때 손쉽게 추적하고 관리할 수 있기 때문입니다. 새로운 프로젝트를 시작할 때는 Fish Shell의 유연한 코드 작성 방식을 활용하여 빠르게 개발할 수 있습니다. 또한 `.fish_project` 파일을 통해 프로젝트 설정을 바꾸거나 프로젝트 실행 스크립트를 수정할 수 있습니다. 더 깊이 알아보고 싶다면 Fish Shell 공식 문서를 참고해보세요!

## See Also
- [Fish Shell 공식 문서 (영문)](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 페이지 (영문)](https://github.com/fish-shell/fish-shell)
- [Fish Shell 설치 방법 (영문)](https://hackernoon.com/up-and-running-with-fish-in-under-5-minutes-21099e83f3b1)