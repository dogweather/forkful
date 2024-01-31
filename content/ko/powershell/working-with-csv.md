---
title:                "CSV 파일 다루기"
date:                  2024-01-19
simple_title:         "CSV 파일 다루기"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
CSV(Comma-Separated Values)는 데이터를 저장하고 공유하기 위한 간단한 형식입니다. 프로그래머들은 효율적인 데이터 교환과 Excel 같은 테이블형 프로그램과의 호환성을 위해 CSV를 자주 사용합니다.

## How to:
### CSV 파일 읽기
```PowerShell
$csvData = Import-Csv -Path "C:\example.csv"
$csvData
```

### CSV 파일 쓰기
```PowerShell
$people = @(
    [PSCustomObject]@{Name="김철수"; Age=30; City="서울"},
    [PSCustomObject]@{Name="이영희"; Age=25; City="부산"}
)
$people | Export-Csv -Path "C:\people.csv" -NoTypeInformation
```

### 결과 출력
```PowerShell
Name    Age    City
----    ---    ----
김철수   30     서울
이영희   25     부산
```

## Deep Dive
CSV는 1970년대 초부터 사용되었으며, 다양한 프로그램들 간의 간단한 데이터 교환 포맷으로 확립되었습니다. XML이나 JSON 같은 형식들도 있지만, 인간이 읽기에는 CSV가 가장 단순하죠. PowerShell에서는 `Import-Csv`와 `Export-Csv` cmdlet을 이용하여 CSV 파일을 쉽게 다룰 수 있으며, 필요에 따라 데이터를 가공하거나, 필터링하는 등의 작업도 가능합니다.

## See Also
- PowerShell Import-Csv documentation: [docs.microsoft.com/Import-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/import-csv)
- PowerShell Export-Csv documentation: [docs.microsoft.com/Export-Csv](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/export-csv)
- CSV 상세 정보: [wikipedia.org/wiki/Comma-separated_values](https://en.wikipedia.org/wiki/Comma-separated_values)
