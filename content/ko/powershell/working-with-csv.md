---
title:                "CSV 파일을 다루는 방법"
html_title:           "PowerShell: CSV 파일을 다루는 방법"
simple_title:         "CSV 파일을 다루는 방법"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇과 왜?
CSV 파일을 다루는 것이란 무엇일까요? 이것은 일반 텍스트 형식으로 이루어진 데이터 파일입니다. 프로그래머들은 일반적으로 CSV 파일을 데이터를 읽고 쓰는데 사용합니다.

## 방법:
```PowerShell
# CSV 파일 읽기
Import-Csv -Path C:\Data.csv

# CSV 파일 쓰기
Export-Csv -Path C:\Data.csv -Encoding UTF8

# CSV 파일을 쓸 때 필드 구분자 지정하기
Export-Csv -Path C:\Data.csv -Delimiter ";"

# CSV 파일을 쓸 때 헤더 추가하기
$data = @( 
    [PSCustomObject]@{ Title = "파일"; Date = "2020/05/15" },
    [PSCustomObject]@{ Title = "문서"; Date = "2020/05/16" }
)
$data | Export-Csv -Path C:\Data.csv -Encoding UTF8 -Append -NoTypeInformation

# CSV 파일을 읽을 때 필요한 필드만 선택하기
Import-Csv -Path C:\Data.csv | Select-Object Title, Date
```

## 심화 살펴보기:
CSV 파일은 1972년에 처음 개발되었으며, 전산 연구실을 위한 데이터 포맷으로 사용되었습니다. 오늘날에도 여전히 유용하게 사용되며, 비슷한 포맷으로는 JSON이 있습니다. CSV 파일을 다룰 때 주의해야 할 점은 필드 구분자나 헤더에 따옴표를 쓰는 규칙이 다를 수 있다는 것입니다.

## 참고 자료:
- [CSV 파일 생성 및 내보내기](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/export-csv)
- [CSV 파일 가져오기 및 내보내기](https://4sysops.com/archives/export-csv-csv-import-csv-powershell-csv/)
- [PowerShell에서 CSV(쉼표로 구분되는 값을 포함하는 직렬 데이터) 가져오기 및 내보내기](https://docs.microsoft.com/ko-kr/previous-versions//bb415435(v=vs.85))