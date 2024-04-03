---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:09.794151-07:00
description: "\uBC29\uBC95: PowerShell\uC740 \uD30C\uC77C\uC744 \uCC98\uB9AC\uD558\
  \uAE30 \uC704\uD55C \uC9C1\uAD00\uC801\uC778 cmdlet\uC744 \uC81C\uACF5\uD569\uB2C8\
  \uB2E4. `Out-File` cmdlet\uACFC \uB9AC\uB2E4\uC774\uB809\uC158 \uC5F0\uC0B0\uC790\
  \uB294 \uC8FC\uB85C \uC774 \uBAA9\uC801\uC744 \uC704\uD574 \uC0AC\uC6A9\uB429\uB2C8\
  \uB2E4. \uB2E4\uC74C\uC740 \uB2E4\uC591\uD55C \uC0C1\uD669\uC5D0\uC11C \uD30C\uC77C\
  \uC5D0 \uD14D\uC2A4\uD2B8\uB97C \uC4F0\uB294 \uBC29\uBC95\uC744 \uC124\uBA85\uD558\
  \uB294 \uC608\uC785\uB2C8\uB2E4: **\uAE30\uBCF8\uC801\uC778 \uD14D\uC2A4\uD2B8 \uD30C\
  \uC77C\u2026"
lastmod: '2024-03-13T22:44:55.573493-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC740 \uD30C\uC77C\uC744 \uCC98\uB9AC\uD558\uAE30 \uC704\uD55C\
  \ \uC9C1\uAD00\uC801\uC778 cmdlet\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC4F0\uAE30"
weight: 24
---

## 방법:
PowerShell은 파일을 처리하기 위한 직관적인 cmdlet을 제공합니다. `Out-File` cmdlet과 리다이렉션 연산자는 주로 이 목적을 위해 사용됩니다. 다음은 다양한 상황에서 파일에 텍스트를 쓰는 방법을 설명하는 예입니다:

**기본적인 텍스트 파일 생성:**

간단한 문자열을 포함하는 텍스트 파일을 만들려면 다음을 사용할 수 있습니다:

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

또는 리다이렉션 연산자를 사용하여 동등하게 작성할 수 있습니다:

```powershell
"Hello, World!" > .\example.txt
```

**기존 파일에 텍스트 추가하기:**

기존 파일의 끝에 텍스트를 추가하고 싶지만 덮어쓰고 싶지 않은 경우:

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

또는 추가 리다이렉션 연산자를 사용하여:

```powershell
"Another line." >> .\example.txt
```

**여러 줄 쓰기:**

여러 줄을 쓰려면 문자열 배열을 사용할 수 있습니다:

```powershell
$lines = "Line 1", "Line 2", "Line 3"
$lines | Out-File -FilePath .\multilines.txt
```

**인코딩 지정하기:**

특정 텍스트 인코딩을 지정하려면 `-Encoding` 매개변수를 사용합니다:

```powershell
"Text with UTF8 Encoding" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**타사 라이브러리 사용하기:**

PowerShell의 내장 cmdlet만으로 기본적인 파일 작업에 충분하지만, 더 복잡한 작업은 `PowershellGet`이나 Windows용으로 포팅된 `SED` 및 `AWK` 같은 타사 모듈이나 도구의 도움을 받을 수 있습니다. 그러나 순수하게 텍스트 파일을 작성하는 것만을 위해서는 이러한 것들이 과한 선택일 수 있으며, 일반적으로 필요하지 않습니다:

```powershell
# 외부 라이브러리 사용이 정당화되는 더 복잡한 시나리오를 가정합니다
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# 여기서 더 복잡한 작업 수행
```

_참고: 타사 의존성을 추가하는 복잡성이 귀하의 요구에 정당한지 항상 고려하세요._

**샘플 출력:**

기본 파일 생성 명령을 실행한 후, `example.txt`의 내용을 확인하면:

```plaintext
Hello, World!
```

텍스트를 추가하고 `example.txt`를 확인하면:

```plaintext
Hello, World!
Another line.
```
