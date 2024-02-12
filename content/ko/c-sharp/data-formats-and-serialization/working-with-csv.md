---
title:                "CSV와 함께 작업하기"
aliases:
- /ko/c-sharp/working-with-csv.md
date:                  2024-02-03T19:19:14.640446-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV와 함께 작업하기"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
CSV(쉼표로 구분된 값) 파일은 표 형식 데이터를 일반 텍스트로 나타내며, 개별 값을 구분하기 위해 쉼표를 사용하는 일반적인 데이터 교환 형식입니다. 프로그래머들은 이를 통해 다양한 응용 프로그램 및 서비스에서 데이터를 쉽게 가져오기, 내보내기 및 조작하기 위해 CSV 파일을 작업합니다. 이 형식은 스프레드시트 응용 프로그램, 데이터베이스 및 프로그래밍 언어와 호환되는 간단하면서 널리 지원되는 형식입니다.

## 사용 방법:
C#에서 CSV 파일을 작업하는 것은 기본 작업을 위해 `System.IO` 네임스페이스를 통해, 그리고 더 복잡한 조작을 하거나 큰 파일을 원활하게 처리하기 위해서는 `CsvHelper`와 같은 타사 라이브러리를 고려할 수 있습니다. 다음은 이러한 접근 방식을 사용하여 CSV 파일을 읽고 쓰는 방법의 예시입니다.

### System.IO를 사용하여 CSV 파일 읽기
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // CSV 파일의 모든 줄을 읽기
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"첫 번째 열: {rowData[0]}, 두 번째 열: {rowData[1]}");
        }
    }
}
```

**샘플 출력:**
```
첫 번째 열: 이름, 두 번째 열: 나이
첫 번째 열: John Doe, 두 번째 열: 30
```

### System.IO를 사용하여 CSV 파일 쓰기
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var lines = new List<string>
        {
            "이름,나이",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSV 파일이 작성되었습니다.");
    }
}
```

**샘플 출력:**
```
CSV 파일이 작성되었습니다.
```

### CsvHelper를 사용하여 CSV 읽기
CsvHelper를 사용하려면, 먼저 NuGet 패키지 관리자를 사용하여 프로젝트에 `CsvHelper` 패키지를 추가합니다.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"첫 번째 열: {record.Name}, 두 번째 열: {record.Age}");
            }
        }
    }
}
```

**샘플 출력:**
```
첫 번째 열: John Doe, 두 번째 열: 30
첫 번째 열: Jane Smith, 두 번째 열: 25
```

### CsvHelper를 사용하여 CSV 쓰기
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { Name = "John Doe", Age = 30 },
            new Person { Name = "Jane Smith", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CsvHelper로 CSV 파일이 작성되었습니다.");
    }
}
```

**샘플 출력:**
```
CsvHelper로 CSV 파일이 작성되었습니다.
```
