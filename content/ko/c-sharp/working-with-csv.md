---
title:                "CSV 파일 다루기"
html_title:           "Arduino: CSV 파일 다루기"
simple_title:         "CSV 파일 다루기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
CSV 파일은 데이터를 저장하는 간단한 형식입니다. 프로그래머들은 구조화된 데이터를 쉽게 교환하고, 다양한 어플리케이션 간 호환성을 위해 CSV를 사용합니다.

## How to:
```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string csvPath = "test.csv";
        // CSV 읽기
        string[] csvLines = File.ReadAllLines(csvPath);
        foreach (var line in csvLines) {
            string[] values = line.Split(',');
            Console.WriteLine($"Name: {values[0]}, Age: {values[1]}");
        }

        // CSV 쓰기
        string[] newLine = { "John Doe", "30" };
        File.AppendAllText(csvPath, string.Join(",", newLine) + Environment.NewLine);
    }
}
```
샘플 출력:
```
Name: Jane Doe, Age: 25
Name: John Doe, Age: 30
```

## Deep Dive (깊은 탐구)
CSV는 최초의 전자 테이블들과 함께 1970년대부터 사용되었습니다. XML이나 JSON 같은 현대적 대안들이 존재하지만, CSV는 그 단순함 덕분에 여전히 인기가 있습니다. C#에서는 File 클래스와 같은 내장 라이브러리를 활용하거나, 더 복잡한 처리가 필요할 경우 CsvHelper와 같은 외부 라이브러리를 사용할 수 있습니다.

## See Also (참고 자료)
- Microsoft의 File 클래스 문서: https://docs.microsoft.com/dotnet/api/system.io.file
- CsvHelper 라이브러리: https://joshclose.github.io/CsvHelper/
- C#에서의 파일 작업 가이드: https://docs.microsoft.com/dotnet/standard/io/how-to-read-text-from-a-file