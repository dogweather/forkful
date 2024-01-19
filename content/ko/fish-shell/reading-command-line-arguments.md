---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

커맨드 라인 인자 읽기는 개발자가 콘솔에서 신호를 프로그램으로 전달할 수 있게 하는 기능입니다. 이는 자동화를 통해 코드를 효과적으로 제어하거나 테스트하고, 사용자 대화형 인터페이스를 구축하는 데 사용됩니다.

## 어떻게:

다음 샘플 코드에서는 Fish shell에서 커맨드 라인 인자를 읽는 방법을 보여줍니다.

```Fish Shell
function hello
    echo Hello, $argv[1]
end

> hello Geeky
Hello, Geeky
```

이 코드는 "hello" 함수를 정의하고, 커맨드 라인 인자를 사용하여 사용자에게 인사를 전달합니다. `$argv[1]`는 첫 번째 인자를 참조합니다.

## 깊이 이해하기:

Fish shell에서 커맨드 라인 인자를 처리하는 방법은 POSIX 쉘 스크립트와 달리, 인덱싱이 1부터 시작합니다. 이 차이점은 언어 설계자들이 1부터 시작하는 인덱싱을 장려하기 위해 Fish shell을 구현하는 동안 도입된 것입니다.

인자를 읽는 대다수의 언어와 플랫폼(예: Python, Java 등)에서 사용자는 개별 인자를 동적으로 설정할 수 있습니다. Fish shell은 이를 통해 사용자가 많은 모듈과 함수를 독립적으로 제어하고 커스텀화할 수 있도록 합니다.

Fish shell의 경우, 커맨드 라인 인자는 `$argv` 변수를 통해 접근할 수 있습니다. 이 변수는 인자 리스트의 배열을 참조합니다.

## 참고 자료:

Fish shell에 대한 자세한 정보를 얻기 위해 다음의 링크들을 참조하세요:

1. Fish Shell 공식 웹사이트: https://fishshell.com
2. Fish Shell 문서: https://fishshell.com/docs/current/tutorial.html
3. Fish Shell GitHub 페이지: https://github.com/fish-shell/fish-shell

Fish shell에 대한 더 진입적인 이해를 얻으려면, 이 주제에 대한 여러 자료를 찾아보는 것이 좋습니다.