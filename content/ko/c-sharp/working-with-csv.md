---
title:                "CSV 파일과 함께 작업하기"
html_title:           "C#: CSV 파일과 함께 작업하기"
simple_title:         "CSV 파일과 함께 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# 왜: CSV 파일을 다루는 것이 왜 중요한지 설명하는 2문장.

## 왜

CSV 파일은 데이터를 편리하게 저장하고 공유할 수 있는 포맷으로 매우 유용합니다. C#을 사용하면 간단하게 CSV 파일을 다룰 수 있습니다.

## 사용 방법

우선 CSV 파일을 읽어오기 위해 `System.IO` 네임스페이스를 사용해야 합니다. 다음 코드는 예시 CSV 파일을 읽어오는 예제입니다.

```C#
using System.IO;

var path = @"C:\Users\User\Desktop\example.csv";

StreamReader reader = new StreamReader(path);

while (!reader.EndOfStream)
{
    var line = reader.ReadLine();
    var values = line.Split(',');

    foreach(var value in values)
    {
        Console.Write(value + " ");
    }
    Console.WriteLine();
}
```

위 코드를 실행하면, CSV 파일의 각 라인을 읽어와서 쉼표 단위로 분리하여 출력합니다. 예를 들어, "1, 2, 3"이라는 라인이 있으면 1 2 3이 출력됩니다. 이렇게 간단하게 CSV 파일의 데이터를 읽어올 수 있습니다.

## 깊게 파헤치기

CSV 파일을 다루는 더 깊은 정보를 알고 싶다면, `System.Data` 네임스페이스에 있는 `DataTable` 클래스를 사용해보세요. 이 클래스를 사용하면 CSV 파일 뿐만 아니라 다양한 데이터 소스에서도 데이터를 읽어올 수 있습니다. 아래 코드는 CSV 파일을 `DataTable` 객체로 읽어오는 예제입니다.

```C#
using System.Data;

var path = @"C:\Users\User\Desktop\example.csv";

DataTable table = new DataTable();

using (StreamReader reader = new StreamReader(path))
{
    string[] headers = reader.ReadLine().Split(',');

    foreach (string header in headers)
    {
        table.Columns.Add(header);
    }

    while (!reader.EndOfStream)
    {
        string[] rowValues = reader.ReadLine().Split(',');
        if (rowValues.Length == table.Columns.Count)
        {
            DataRow row = table.NewRow();
            row.ItemArray = rowValues;
            table.Rows.Add(row);
        }
    }
}

// DataTable 객체를 이용해 데이터를 조작하거나 출력할 수 있습니다.
table.Rows[0]["Name"] = "John";
// ...

// DataTable 객체를 CSV 파일로 내보내는 예제입니다.
string csv = string.Join(Environment.NewLine, table.Rows
            .Cast<DataRow>()
            .Select(row => string.Join(",", row.ItemArray)));
File.WriteAllText(@"C:\Users\User\Desktop\example_output.csv", csv);
```

`DataTable` 클래스를 사용하면 CSV 파일의 데이터를 자유롭게 다룰 수 있습니다. 더 많은 정보는 MSDN 문서를 참고하시면 됩니다.

## 자세히 알아보기

[Introduction to Reading and Writing CSV Files in C#](https://www.codeproject.com/Tips/1169109/Introduction-to-Reading-and-Writing-CSV-Files-in) - 간단한 CSV 파일 읽기/쓰기 예제

[Working with CSV Files Using C# and .NET](https://blogs.msmvps.com/deborahk/working-with-csv-files-using-c-and-net/) - `DataTable` 클래스를 사용한 CSV 파일 다루기 예제

## 다른 참고 자료

- [C# Programming - A Beginner's Course](https://www.udemy.com/course/c-sharp-programming-for-beginners/) - C# 프로그래밍 초보자를 위한 강의
- [C# and .NET Learning Resources](https://docs.microsoft.com/en-us/dotnet/csharp/learning-resources/) - C#과 .NET 관련 공식 학습 자료