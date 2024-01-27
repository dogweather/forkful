---
title:                "CLI 한 줄 명령어로 파일 수정하기"
date:                  2024-01-26T22:25:20.221118-07:00
model:                 gpt-4-0125-preview
simple_title:         "CLI 한 줄 명령어로 파일 수정하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

파워셸에서 명령 줄 인터페이스(CLI) 원 라이너를 사용한 파일 수정은 파일을 직접 편집, 변환 또는 업데이트하기 위해 간결한 명령을 사용하는 것입니다. 프로그래머들은 그래픽 편집기에서 파일을 열지 않고도 빠르게 변경사항을 적용하기 위해 이 방법을 사용하며, 워크플로우를 가속화하고 반복 작업을 자동화할 수 있습니다.

## 방법:

파일 내 특정 문자열을 대체하려면 `Get-Content`과 `Set-Content` cmdlet을 `ForEach-Object` cmdlet과 함께 사용할 수 있습니다:

```PowerShell
Get-Content ./example.txt | ForEach-Object { $_ -replace 'oldString', 'newString' } | Set-Content ./example.txt
```

파일 끝에 줄을 추가하려면 `Add-Content` cmdlet을 사용할 수 있습니다:

```PowerShell
Add-Content ./example.txt "This is the new line at the end of the file."
```

파일에서 빈 줄을 제거하고 싶다면, PowerShell은 간단하게 만들어 줍니다:

```PowerShell
Get-Content ./example.txt | Where-Object { $_.Trim() -ne '' } | Set-Content ./cleaned_example.txt
```

빈 줄 제거에 대한 예시 출력은 `cleaned_example.txt`의 내용이 `example.txt`에 있던 빈 줄이나 공백만 있는 줄을 제외한 내용일 것입니다.

## 심층 분석

파워셸에서 CLI 원 라이너를 사용한 파일 수정의 강점은 .NET 프레임워크에 기반한 광범위한 cmdlet 세트에서 비롯됩니다. 이는 강력한 기능 세트를 제공합니다. 이 방법은 하나의 작업을 잘 수행하는 간단한 도구를 만드는 Unix 철학을 되새김질하며, 파워셸은 단일 셸 내에서 다재다능한 툴킷을 제공함으로써 이 원칙을 확장합니다.

이 작업에 대한 파워셸의 대안으로는 Bash와 같은 환경에서 `sed`, `awk`, `grep`과 같은 Unix 기반 도구를 사용하는 것이 있습니다. 이 도구들은 매우 효율적이며 수십 년 동안 Unix/Linux 시스템에서 파일 조작을 위한 가장 적합한 솔루션이었습니다. 그러나 파워셸의 접근 방식은 Windows 객체 모델과 긴밀하게 통합되어 Windows 환경에서 독특한 이점을 제공합니다.

주목할 중요한 구현 세부 사항은 파워셸이 파일 내용을 메모리에서 처리한다는 것으로, 이는 Unix/Linux의 일부 스트림 지향 도구에 비해 매우 큰 파일을 처리하는 데 있어 덜 효율적일 수 있습니다. 또한, 파워셸의 장황함이 스크립트를 읽기 쉽게 만들지만 때로는 Unix 대응물에 비해 길어지는 원 라이너를 초래할 수 있습니다. 그러나 Windows 중심 환경과 Windows 생태계와의 깊은 통합에서 이점을 얻는 작업의 경우, 파워셸은 비교할 수 없는 기능을 제공합니다.

## 참고 자료

파워셸에서 파일 조작의 더 복잡한 예시와 추가적인 독서 자료는 다음과 같이 도움이 될 수 있습니다:

- 공식 파워셸 문서, cmdlet에 대한 종합적인 가이드를 제공합니다: [https://docs.microsoft.com/ko-kr/powershell/](https://docs.microsoft.com/ko-kr/powershell/)
- Ed Wilson이 작성한 "PowerShell Scripting Guide"는 스크립팅, 파일 조작 작업을 포함하여 심층적인 토론과 예시를 제공합니다.
- Unix 배경에서 오신 분들이나 교차 호환성에 관심이 있는 경우, "리눅스 관리자를 위한 파워셸 배우기"는 다양한 운영 체제에서 파워셸의 강력함을 이해하는데 훌륭한 자료입니다.
