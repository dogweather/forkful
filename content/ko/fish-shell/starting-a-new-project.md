---
title:                "Fish Shell: 새 프로젝트 시작하기"
programming_language: "Fish Shell"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 왜 새 프로젝트를 시작해야 할까요?

새로운 프로젝트를 시작하는 것은 프로그래밍을 즐기는 사람들에게는 무척 흥미로운 일일 수 있습니다. 새로운 아이디어를 구현하고 새로운 도구를 사용하여 프로그래밍 기술을 연마하는 것은 늘 즐거운 일입니다. 또한 새로운 프로젝트를 시작하면 이전에 없던 새로운 기술들을 배우고 새로운 문제들에 도전할 수 있게 됩니다.

## 시작하는 방법

새로운 프로젝트를 시작하는 첫 단계는 그 프로젝트를 실행할 수 있는 환경을 설정하는 것입니다. 우리는 Fish Shell을 사용하여 언어 간의 관용적인 명령어를 사용하는 것을 권장합니다. 이를 통해 보다 효율적이고 빠른 개발을 할 수 있습니다.

```Fish Shell

set -U PROJECT_NAME myProject # 새 프로젝트의 이름 설정

cd ~ # 홈 디렉토리로 이동

mkdir $PROJECT_NAME # 프로젝트 디렉토리 생성

cd $PROJECT_NAME # 프로젝트 디렉토리로 이동

```

이제 새로운 프로젝트 디렉토리에서 새 파일을 만들고 프로그래밍을 시작할 준비가 되었습니다. Fish Shell을 사용하면 자신만의 프로젝트 스크립트를 만들 수 있으며 이를 통해 간단한 작업도 쉽게 처리할 수 있습니다.

```Fish Shell

touch main.py # 빈 Python 파일 생성

echo "print('Hello World')" > main.py # 파일에 코드 입력

python3 main.py # 코드 실행

```

## 딥 다이브

새로운 프로젝트를 시작하는 것이 멋진 일이지만 실제로 개발을 시작하기 위해서는 몇 가지 고려해야 할 사항들이 있습니다. 첫째로, 프로젝트의 목적을 명확하게 정의해야 합니다. 무엇을 만들고자 하는지, 어떤 언어와 도구를 사용할 것인지 등을 명확하게 결정해야 합니다. 둘째로, 개발 프로세스를 계획해야 합니다. 프로젝트를 어떤 순서로 진행할 것인지, 어떤 기간 내에 완료할 것인지 등을 계획하는 것이 중요합니다. 마지막으로, 코드의 유지보수를 위해 좋은 코딩 습관을 가지고 프로젝트를 진행해야 합니다.

# 관련 링크

[Korean Fish Shell 사용자 가이드](https://medium.com/consolemorning/fish-shell-사용자-가이드-d03af6511305)

[Fish Shell 공식 홈페이지](https://fishshell.com/)

[새 프로젝트를 위한 핵심 고려사항](https://www.codeproject.com/articles/826299/things-to-consider-before-starting-a-new-project)

# 더 보기