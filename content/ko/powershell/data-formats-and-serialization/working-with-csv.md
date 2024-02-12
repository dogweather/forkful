---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/powershell/working-with-csv.md
date:                  2024-02-03T19:20:50.815471-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV(쉼표로 구분된 값) 파일을 다루는 것은 구조화된, 표형식의 데이터를 관리하고 조작하는 일반적인 작업입니다. 프로그래머들은 다양한 응용 프로그램, 예를 들어 데이터 분석, 보고 또는 웹 어플리케이션을 위한 데이터를 효율적으로 가져오기, 내보내기 또는 조작하기 위해 종종 이 작업을 수행합니다.

## 어떻게:

### CSV 파일 읽기

CSV 파일에서 읽으려면 `Import-Csv` cmdlet을 사용하세요. 이 cmdlet은 파일을 읽고 각 행에 대해 사용자 지정 PowerShell 객체로 변환합니다.

```powershell
# CSV 파일 가져오기
$data = Import-Csv -Path "C:\Data\users.csv"
# 내용 표시
$data
```

**샘플 출력:**

```
Name    Age    City
----    ---    ----
John    23     New York
Doe     29     Los Angeles
```

### CSV 파일에 쓰기

반대로, CSV 파일에 데이터를 쓰려면 `Export-Csv` cmdlet이 사용됩니다. 이 cmdlet은 입력 객체를 CSV 형식으로 변환합니다.

```powershell
# 내보낼 객체 생성
$users = @(
    [PSCustomObject]@{Name='John'; Age='23'; City='New York'},
    [PSCustomObject]@{Name='Doe'; Age='29'; City='Los Angeles'}
)

# CSV 파일로 내보내기
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

실행 후, 제공된 데이터를 포함한 `new_users.csv`라는 파일이 생성됩니다.

### CSV 내용 필터링 및 조작하기

CSV 파일의 데이터를 필터링하거나 조작하려면 PowerShell의 객체 조작 기능을 사용하세요. 예를 들어, 특정 나이 이상이고 특정 도시 출신의 사용자만 선택하려면:

```powershell
# 데이터 가져오기 및 필터링
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Age -gt 25 -and $_.City -eq 'Los Angeles'
}

# 필터링된 데이터 표시
$filteredData
```

**샘플 출력:**

```
Name    Age    City
----    ---    ----
Doe     29     Los Angeles
```

### 서드파티 라이브러리 사용하기

PowerShell의 기본 cmdlet만으로도 일반적인 작업에는 충분하지만, 더 복잡한 작업은 서드파티 라이브러리나 도구의 도움을 받을 수도 있습니다. 그러나 CSV 조작, 예를 들어 읽기, 쓰기, 필터링 또는 정렬과 같은 표준 작업에 대해서는 `Import-Csv`와 `Export-Csv`와 같은 PowerShell의 내장 cmdlet이 추가 라이브러리 없이도 강력한 기능을 제공하는 경우가 많습니다.
