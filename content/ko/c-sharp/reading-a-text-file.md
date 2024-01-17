---
title:                "텍스트 파일 읽기"
html_title:           "C#: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

텍스트 파일을 읽는 것은 컴퓨터 프로그래머들이 사용하는 기술 중 하나입니다. 이는 텍스트 파일의 내용을 읽어와서 컴퓨터에서 이해할 수 있는 형태로 변환하는 과정입니다. 프로그래머들은 이를 통해 파일에 저장된 정보를 처리하고 활용할 수 있게 됩니다.

## 방법:

### 파일 전체 읽기:

```C#
string filePath = "textfile.txt";
string rawText = System.IO.File.ReadAllText(filePath);
Console.WriteLine(rawText);
```
**출력:**
```Hello, world!```

### 텍스트 한 줄씩 읽기:

```C#
string filePath = "textfile.txt";
string[] lines = System.IO.File.ReadAllLines(filePath);
foreach (string line in lines)
{
    Console.WriteLine(line);
}
```
**출력:**
```
Hello,
world!
```

## 더 깊게 들어가보기:

### 역사적 배경:

텍스트 파일을 읽는 기술은 컴퓨터의 초기부터 사용되어 온 기술 중 하나입니다. 당시에는 파일의 내용을 메모리에 로드한 뒤 이를 처리하는 방식을 사용했으며, 현재는 텍스트 파일 자체를 읽는 방식이 더 선호되고 있습니다.

### 대안:

텍스트 파일을 읽는 다른 방법으로는 ```StreamReader``` 클래스를 사용하는 것이 있습니다. 이 클래스는 메모리 사용량이 적고 텍스트 파일의 크기가 큰 경우에 유용합니다. 또한 파일의 크기가 너무 크거나 시간이 제한적인 경우에는 텍스트 파일이 아닌 데이터베이스 등 다른 형식을 사용하는 것이 일반적입니다.

### 구현 세부사항:

```C#```에서는 ```System.IO``` 네임스페이스를 사용하여 텍스트 파일을 읽는 기능을 제공하고 있습니다. 이를 사용하기 위해서는 해당 네임스페이스를 호출하고, 파일 경로를 지정하여 파일을 열어야 합니다. 읽은 내용은 내부적으로 스트링 형태로 저장되며, 이를 바탕으로 원하는 방식으로 처리할 수 있습니다.

## 참고 자료:

- [StreamReader 클래스, Microsoft Docs](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader?view=net-5.0)
- [System.IO 네임스페이스, Microsoft Docs](https://docs.microsoft.com/ko-kr/dotnet/api/system.io?view=net-5.0)