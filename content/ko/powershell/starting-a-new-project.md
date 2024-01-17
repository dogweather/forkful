---
title:                "새 프로젝트 시작하기"
html_title:           "PowerShell: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

새 프로젝트를 시작한다는 것은 새로운 소프트웨어나 애플리케이션을 만들기 위한 작업입니다. 프로그래머들은 새 프로젝트를 시작하는 이유는 종종 배움과 도전을 위해서입니다. 새로운 도구를 사용하고 새로운 기술을 익히며, 더 나은 코드를 작성하기 위해서 말이죠.

# 어떻게:

```PowerShell
# 새 프로젝트 폴더를 만들기
New-Item -ItemType Directory -Path C:\Projects\NewProject

# 새 프로젝트에 필요한 파일 생성
New-Item -ItemType File -Path C:\Projects\NewProject\index.html
New-Item -ItemType File -Path C:\Projects\NewProject\style.css
New-Item -ItemType File -Path C:\Projects\NewProject\script.js

# 새 프로젝트로 이동하기
Set-Location -Path C:\Projects\NewProject
```

위의 코드는 새 프로젝트를 만들고 관련 파일을 생성하고, 만들어진 폴더로 이동하는 과정을 보여줍니다.

# 깊이 살펴보기:

새 프로젝트를 시작하기 전에, 프로그래머는 다양한 방법으로 새 프로젝트를 만들 수 있습니다. PowerShell을 사용하는것 외에도, 내장된 Windows 기능이나 다른 스크립팅 언어를 사용할 수도 있습니다. 특히 PowerShell을 사용하는 것은 매우 간단하고 강력합니다.

# 관련 문서:

- [PowerShell 공식 홈페이지](https://docs.microsoft.com/ko-kr/powershell/)에서 PowerShell의 다양한 기능을 익힐 수 있습니다.
- [Windows 스크립팅](https://docs.microsoft.com/ko-kr/windows-server/administration/windows-commands/index)을 활용하여 PowerShell보다 더 다양한 명령어를 사용할 수 있습니다.