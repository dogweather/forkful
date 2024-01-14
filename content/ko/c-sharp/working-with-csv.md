---
title:                "C#: csv로 작업하기"
simple_title:         "csv로 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# 왜

CSV 파일과 함께 작업하는 이유는 데이터를 효율적으로 저장하고 관리하기 위해서 입니다.

# 방법

CSV 파일은 일반적으로 콤마로 구분된 데이터를 포맷한 파일입니다. 이러한 파일은 주로 엑셀 또는 데이터베이스에 쉽게 가져올 수 있으며, C#을 사용하여 CSV 파일을 쉽게 다룰 수 있습니다. 아래는 CSV 파일을 C#으로 읽고 쓰는 예제 코드입니다.

```C#
// CSV 파일 읽기
using (var reader = new StreamReader("file.csv"))
{
    while (!reader.EndOfStream)
    {
        var line = reader.ReadLine();
        var data = line.Split(','); // 콤마를 기준으로 데이터를 나눠줍니다.
        // 데이터 사용
        Console.WriteLine(data[0]); // 첫번째 열 출력
        Console.WriteLine(data[1]); // 두번째 열 출력
    }
}

// CSV 파일 쓰기
using (var writer = new StreamWriter("new_file.csv"))
{
    var data = new List<string[]>();
    data.Add(new string[] { "Name", "Age" }); // 열 이름 추가
    data.Add(new string[] { "John", "25" }); // 데이터 추가
    data.Add(new string[] { "Sarah", "30" }); // 데이터 추가
    foreach (var row in data)
        writer.WriteLine(string.Join(",", row)); // 콤마로 구분하여 CSV 파일에 데이터 쓰기
}
```

위의 예제 코드에서는 CSV 파일을 읽고 쓰는 간단한 방법을 알 수 있습니다. 이 외에도 C# 라이브러리를 사용하면 더 다양한 기능을 구현할 수 있습니다.

# 깊이있게 알아보기

CSV 파일을 다루는 라이브러리는 다양한 형태로 존재합니다. 가장 대표적인 것은 Microsoft가 제공하는 "CsvHelper" 라이브러리입니다. 이 라이브러리를 사용하면 CSV 파일을 쉽게 다룰 수 있을 뿐만 아니라 데이터 타입 변환 및 유효성 검사 등 다양한 기능을 제공합니다.

또한, CSV 파일을 다룰 때 주의해야 할 점도 있습니다. CSV 파일은 데이터를 쉼표로 구분하기 때문에, 만약 데이터에 쉼표가 포함된 경우에는 적절한 처리가 필요합니다. 또한, 한글 인코딩 문제나 대용량 데이터 처리 문제 등에도 유의해야 합니다.

# 참고

[Microsoft Docs - File.ReadAllBytes 메서드](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.file.readallbytes?view=net-5.0)

[CsvHelper 라이브러리](https://joshclose.github.io/CsvHelper/)

[CSV파일 한글 인코딩 문제 해결](https://xodud0414.tistory.com/231)

See Also: 데이터 파일 형식에 대한 이해