---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇이고 왜합니까?
새 프로젝트를 시작하는 것은 빈 화이트보드에서 모든 것을 구성하는 것과 같습니다. 프로그래머는 새로운 문제를 해결하거나, 기존의 방식보다 더 나은 방법으로 프로세스를 개선하는데 이것이 필요합니다.

## 어떻게:
자, PowerShell에서 새 프로젝트를 시작하는 방법을 살펴 보겠습니다. 기본적인 뼈대를 시작으로 해봅시다.

```PowerShell
# 새 항목 만들기
New-Item -Path 'C:\MyProject' -ItemType Directory
```

이러한 명령은 `MyProject` 라는 새로운 디렉토리를 C 드라이브에 만듭니다.

## Deep Dive:
"새 프로젝트 시작", 이 아이디어는 컴퓨터 과학의 초기 시점부터 존재했습니다. 처음에는 개발자들이 매번 새로운 프로그램을 작성해서 실행시켰습니다. 그러나 누구나 코딩을 처음부터 다시 시작하는 것은 효율적이지 않다는 것을 알게되었고, 그 때프로 깨닳게 된것이 파일 시스템과 디렉토리입니다.

PowerShell의 대안 중 하나는 Bash Scripting입니다. Linux와 Mac 환경에서 주로 사용됩니다. 그러나 PowerShell은 .Net Framework와 Windows 플랫폼과의 통합을 강화하므로 Windows 개발자에게는 더 적합합니다.

결론적으로 말하면, 디렉토리를 초기화하고 깨끗한 작업 공간을 만드는 것은 새로운 프로젝트 시작의 핵심입니다. 

## 참고하기:
1. PowerShell 예제와 튜토리얼: https://docs.microsoft.com/en-us/powershell/scripting/overview?view=powershell-7.1
2. Bash Scripting 가이드: https://www.gnu.org/software/bash/manual/bash.html
3. 프로젝트 관리의 기초: https://www.pmi.org/learning/library/project-management-basics-an-official-pmi-guide-11569