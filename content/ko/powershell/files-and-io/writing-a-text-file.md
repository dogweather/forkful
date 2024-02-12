---
title:                "텍스트 파일 쓰기"
aliases:
- /ko/powershell/writing-a-text-file.md
date:                  2024-02-03T19:29:09.794151-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
PowerShell에서 텍스트 파일을 작성하는 것은 로깅, 데이터 저장, 구성 스크립팅을 위한 기본적인 작업으로 텍스트 기반 파일을 만들고 조작하는 것을 포함합니다. 프로그래머들은 이를 시스템 작업 자동화, 데이터 분석, 다른 애플리케이션이나 스크립트와의 통합을 위해 활용합니다.

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
