---
title:                "CLI 명령어로 파일 다루기"
aliases:
- /ko/powershell/manipulating-files-with-cli-one-liners.md
date:                  2024-01-27T16:20:45.194990-07:00
model:                 gpt-4-0125-preview
simple_title:         "CLI 명령어로 파일 다루기"

tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/manipulating-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

파워쉘에서 CLI 한 줄 명령어를 사용한 파일 조작은 명령 줄을 직접 사용하여 파일 데이터를 신속하게 변경, 이동 또는 획득하는 것에 관한 것입니다. 프로그래머들은 이를 효율성을 위해 사용합니다; 간단한 작업에 대해 긴 스크립트를 작성하거나 GUI를 탐색하는 것보다 빠릅니다.

## 사용 방법:

### 파일 읽기
파일의 내용을 신속하게 표시하려면 `Get-Content` 명령을 사용하세요:
```PowerShell
Get-Content .\example.txt
```

### 파일에 쓰기
파일에 새로운 것을 쓰려면 `Set-Content`을 사용할 수 있습니다:
```PowerShell
Set-Content -Path .\example.txt -Value "Hello, PowerShell!"
```

### 파일에 추가하기
파일 내용을 지우지 않고 파일 끝에 데이터를 추가하려면 `Add-Content`을 사용함:
```PowerShell
Add-Content -Path .\example.txt -Value "Adding this line."
```

### 파일 복사하기
`Copy-Item`으로 파일을 복사하는 것은 간단합니다:
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### 파일 삭제하기
파일을 제거하려면 `Remove-Item`을 간단히 사용하세요:
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### 파일 내에서 검색하기
파일 내의 텍스트를 검색하려면 `Select-String`을 사용하세요:
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### 명령어 결합하기
파워쉘은 명령어를 파이프로 연결하여 체인하는 능력으로 진정 빛납니다. 새 디렉토리로 파일을 찾아 복사하는 방법은 다음과 같습니다:
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## 심층 분석

역사적으로, 파워쉘은 전통적인 윈도우 명령 프롬프트에 대한 보다 강력한 대안으로 소개되었으며, 시스템 내부와 데이터 저장소에 전례 없는 접근을 제공합니다. 명령줄 속도와 스크립팅의 유연성을 결합하여, 윈도우 기반 시스템 관리자와 개발자 모두에게 없어서는 안될 도구로 만듭니다.

파워쉘에 대한 대안으로 파일 조작에는 유닉스 기반 도구인 `sed`, `awk`, `grep`, 그리고 리눅스 및 MacOS 사용자를 위한 `bash` 스크립팅 등이 있습니다. 이 도구들은 매우 강력하고 각자의 장점이 있지만, 파워쉘은 윈도우 환경과의 깊은 통합을 제공합니다.

파워쉘의 주목할 만한 측면은 그것의 객체 지향적 특성입니다. 많은 스크립팅 언어가 모든 것을 문자열이나 바이트의 스트림으로 취급하는 반면, 파워쉘은 .NET 객체로 직접 작업합니다. 이는 파일을 조작할 때 풍부한 객체를 사용한다는 것을 의미하며, 이러한 객체는 다양한 속성과 메서드를 제공하여 복잡한 작업을 관리하기 쉽게 만들어 줍니다.

리눅스와 MacOS 사용자에게 파워쉘의 약점 중 하나는 bash 스크립팅이나 유닉스 명령줄 도구를 사용하는 것에 비해 지각된 장황함일 수 있습니다. 또한, 파워쉘의 윈도우와의 깊은 통합은 가끔 크로스 플랫폼 스크립트를 약간 더 도전적으로 만들 수 있으며, 파워쉘 코어와 함께하는 노력은 이 격차를 효과적으로 좁히고자 합니다.

그 약점에도 불구하고, 파워쉘의 강점은 강력한 한 줄 명령어 기능, 통합 스크립팅 환경, 그리고 윈도우 생태계에 대한 광범위한 접근을 제공하여, 명령 줄에서 직접 파일 조작과 훨씬 더 많은 것을 하기를 원하는 이들에게 필수적인 도구로 만듭니다.
