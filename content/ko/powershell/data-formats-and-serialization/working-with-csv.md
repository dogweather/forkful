---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:50.815471-07:00
description: "\uC5B4\uB5BB\uAC8C: CSV \uD30C\uC77C\uC5D0\uC11C \uC77D\uC73C\uB824\uBA74\
  \ `Import-Csv` cmdlet\uC744 \uC0AC\uC6A9\uD558\uC138\uC694. \uC774 cmdlet\uC740\
  \ \uD30C\uC77C\uC744 \uC77D\uACE0 \uAC01 \uD589\uC5D0 \uB300\uD574 \uC0AC\uC6A9\uC790\
  \ \uC9C0\uC815 PowerShell \uAC1D\uCCB4\uB85C \uBCC0\uD658\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.579657-06:00'
model: gpt-4-0125-preview
summary: "CSV \uD30C\uC77C\uC5D0\uC11C \uC77D\uC73C\uB824\uBA74 `Import-Csv` cmdlet\uC744\
  \ \uC0AC\uC6A9\uD558\uC138\uC694."
title: "CSV\uC640 \uD568\uAED8 \uC791\uC5C5\uD558\uAE30"
weight: 37
---

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
