---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

신규 프로젝트를 시작하는 것은 개발자가 새로운 아이디어나 기능을 구현하기 위해 새롭게 개발 환경을 설정하는 것을 말합니다. 개발자들은 이를 통해 해당 프로젝트에 딱맞는 도구와 환경을 구축하고, 더 효율적으로 목표를 달성하기 위해 이를 수행합니다.

## 어떻게 하는가:

쉘 스크립트를 작성시 `#!/bin/bash` 표기는 반드시 제일 처음에 위치해야 합니다. 이는 스크립트의 실행 환경을 bash로 설정하기 위함입니다.

```Bash
#!/bin/bash
echo "Hello, New Project!"
```

하면 터미널 화면에 'Hello, New Project!'라고 출력되게 됩니다.

## 깊이 파보기:

1. 역사적 맥락: bash는 1989년 인기있던 sh 쉘의 자유 소프트웨어 버전으로 등장해, 오늘날 가장 널리 사용되는 UNIX shell 중 하나입니다.

2. 대안: bash와 동등한 기능성을 가진 다른 쉘로는 Zsh, Fish 등이 있습니다. 각자 특징을 가지고 있으니 상황에 따라 선택하시면 됩니다.

3. 구현 정보: bash 스크립트에서는 변수를 활용해 반환 값을 전달하거나, 스크립트의 실행 지연을 위해 sleep 함수를 사용할 수 있습니다.

```Bash
#!/bin/bash
VAR="Hello, New Project!"
echo $VAR
sleep 2
```

이 스크립트는 "Hello, New Project!"를 출력하고, 2초 후에 종료됩니다.

## 참고 자료:

- Bash Scripting Guide(Korean): http://wiki.kldp.org/KoreanDoc/html/Bash-Beginners-Guide-KLDP/
- Bash 쉘 스크립트 문법: http://zetawiki.com/wiki/Bash_-n,_-z,_-eq,_-ne,_-lt,_-le,_-gt,_-ge
- Bash vs Zsh vs Fish comparison: https://linuxhint.com/bash_vs_zsh_vs_fish/