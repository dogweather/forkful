---
title:                "텍스트 파일 작성하기"
aliases:
- /ko/vba/writing-a-text-file.md
date:                  2024-02-01T22:08:55.287655-07:00
model:                 gpt-4-0125-preview
simple_title:         "텍스트 파일 작성하기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/writing-a-text-file.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 텍스트 파일을 작성하는 것은 파일에 텍스트 데이터를 생성, 수정, 또는 추가하는 것을 포함하며, 이는 출력 저장, 로깅, 다른 응용 프로그램과의 상호 작용을 위한 기본적인 작업입니다. 프로그래머들은 이 기능을 사용하여 Microsoft Office 생태계 내에서 보고서 자동화, 데이터 내보내기 또는 구성 파일 생성을 자동화합니다.

## 방법:

VBA는 파일에 쓰는 여러 가지 방법을 제공하지만 가장 간단한 방법 중 하나는 `FileSystemObject`를 사용하는 것입니다. 간단한 텍스트 파일을 생성하고 그것에 데이터를 작성하는 단계별 가이드는 다음과 같습니다:

1. **Microsoft Scripting Runtime 참조**: 먼저, VBA 에디터가 `FileSystemObject`에 접근할 수 있는지 확인합니다. VBA 에디터에서 도구 > 참조로 이동하여 "Microsoft Scripting Runtime"을 체크합니다.

2. **텍스트 파일 생성**: 다음 VBA 코드 스니펫은 텍스트 파일을 생성하고 그 안에 한 줄의 텍스트를 작성하는 방법을 보여줍니다.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' CreateTextFile 매개변수: (파일명, 덮어쓰기, 유니코드)
    Set textFile = fso.CreateTextFile("C:\yourPath\example.txt", True, False)
    
    ' 텍스트 한 줄 작성
    textFile.WriteLine "Hello, VBA!"
    
    ' 파일 닫기
    textFile.Close
End Sub
```

이 스크립트는 지정된 디렉토리에 `example.txt`라는 파일을 생성(또는 이미 존재하는 경우 덮어쓰기)하고 "Hello, VBA!"를 작성한 후 파일을 닫아 변경사항을 저장합니다.

3. **샘플 출력**:

위의 VBA 스크립트를 실행한 후, 다음 내용을 포함하는 `example.txt`라는 파일을 찾을 수 있습니다:

```
Hello, VBA!
```

## 심층 분석:

`FileSystemObject`(FSO)는 Microsoft Scripting Runtime 라이브러리의 일부로, 파일 작업을 위한 풍부한 속성 및 메서드 집합을 제공하며, VBA의 전통적인 파일 처리 기능(`Open`, `Print` #, `Write` # 등)을 넘어섭니다. 파일 처리뿐만 아니라 FSO는 폴더와 드라이브도 조작할 수 있으므로 VBA 내에서 파일 시스템 작업을 위한 강력한 도구입니다.

그러나 FSO는 VBA의 기본 파일 처리문보다는 보다 현대적인 접근 방식을 제시하지만, 간단한 작업에 대한 오버헤드를 도입할 수 있음을 주목하는 것이 중요합니다. 또한, FSO는 외부 라이브러리의 일부이므로 다른 시스템(예: Office의 이전 버전, Mac Office)과의 호환성과 이식성이 우려될 수 있습니다.

성능, 호환성 또는 최소한의 외부 의존성이 중요한 상황에서는 프로그래머들이 VBA의 내장 파일 처리 기술을 사용할 수 있습니다. 그러나, 보다 복잡한 작업을 수행하거나 이러한 우려 사항이 완화된 환경(예: 통제된 기업 환경)에서 작업할 때는 FileSystemObject의 이점이 그 단점을 종종 상쇄합니다.
