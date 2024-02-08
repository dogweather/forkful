---
title:                "로깅"
date:                  2024-02-01T21:56:04.548731-07:00
model:                 gpt-4-0125-preview
simple_title:         "로깅"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/vba/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

Visual Basic for Applications(VBA)에서 로깅은 프로그램의 런타임 동작에 대한 정보를 파일, 콘솔 또는 데이터베이스에 기록하는 것을 말합니다. 프로그래머들은 로깅을 통해 애플리케이션을 모니터링하고, 문제를 진단하며, 성능 특성을 이해합니다.

## 방법:

VBA에는 다른 언어에서 찾을 수 있는 내장 로깅 프레임워크가 없습니다. 그러나, 간단한 로깅 메커니즘을 구현하는 것은 간단합니다. 아래는 기본 파일 로거를 만드는 방법의 예입니다.

1. **로그 파일에 쓰기**: 이 예제 함수인 `LogMessage`는 타임스탬프와 함께 메시지를 텍스트 파일에 씁니다.

```basic
Sub LogMessage(message As String)
    Dim logFilePath As String
    Dim fileNum As Integer
    
    ' 로그 파일의 경로를 지정합니다
    logFilePath = ThisWorkbook.Path & "\log.txt"
    
    ' 다음 사용 가능한 파일 번호를 가져옵니다
    fileNum = FreeFile()
    
    ' 파일을 추가 모드로 엽니다
    Open logFilePath For Append As #fileNum
    
    ' 타임스탬프와 로그 메시지를 기록합니다
    Print #fileNum, Now & ": " & message
    
    ' 파일을 닫습니다
    Close #fileNum
End Sub
```

메시지를 로깅하려면 단순히 `LogMessage("여기에 메시지를 입력하세요")`를 호출하면 됩니다. 이렇게 하면 *log.txt*에 다음과 같은 항목이 생성됩니다:

```
2023년 4월 30일 오후 3:45:32: 여기에 메시지를 입력하세요
```

2. **로그 파일에서 읽기**: 로그 파일의 내용을 읽고 표시하려면:

```basic
Sub ReadLogFile()
    Dim logFilePath As String
    Dim fileContent As String
    Dim fileNum As Integer
    
    logFilePath = ThisWorkbook.Path & "\log.txt"
    fileNum = FreeFile()
    
    ' 파일을 읽기 모드로 엽니다
    Open logFilePath For Input As #fileNum
    
    ' 파일의 전체 내용을 읽습니다
    fileContent = Input(LOF(fileNum), fileNum)
    
    ' 파일을 닫습니다
    Close #fileNum
    
    ' 파일 내용을 표시합니다
    MsgBox fileContent
End Sub
```

## 심층 분석

VBA에서는 기본 로깅 프레임워크가 없기 때문에, 일반적으로 기본 파일 작업을 통해 로깅을 구현하거나 데이터베이스에 로깅하거나 Windows 이벤트 로그와 상호 작용하는 등 더 고급 요구 사항을 위해 외부 COM 객체의 힘을 활용합니다. 역사적으로, VBA에서의 로깅은 그것의 단순한 에러 처리 및 디버깅 도구에 의해 제기된 한계를 우회하는 방법이었습니다. 효과적이긴 하지만, 로깅을 위한 직접적인 파일 조작은 큰 데이터 양이나 높은 동시성 하에서 비효율적일 수 있습니다. 보다 정교한 로깅 기능을 위해, 프로그래머들은 종종 외부 라이브러리로 전환하거나 ELK 스택(Elasticsearch, Logstash, Kibana)이나 Splunk와 같이 로깅을 위해 특별히 설계된 시스템과 웹 서비스 호출이나 중개 데이터베이스를 통해 통합합니다. VBA가 새로운 프로그래밍 언어에서 찾을 수 있는 현대적인 편의성을 제공하지는 않지만, 그것의 기능과 한계를 이해하는 것은 프로그래머들이 애플리케이션 모니터링 및 진단을 위한 강력한 도구로 로깅을 효과적으로 활용할 수 있게 해줍니다.
