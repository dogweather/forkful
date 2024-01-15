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

## 왜

이번 글에서 우리는 C# 언어를 사용하여 텍스트 파일을 읽는 방법에 대해 알아보겠습니다. 텍스트 파일을 읽는 것은 프로그래밍에서 매우 중요한 기능이며, 실제 프로젝트에서도 자주 사용됩니다. 따라서 C# 개발을 하시는 분들에게는 꼭 필요한 내용입니다.

## 어떻게 하나요?

```C#
// 파일 경로를 지정해줍니다.
string path = "textfile.txt";

// StreamReader 객체를 생성합니다.
StreamReader reader = new StreamReader(path);

// 파일의 모든 내용을 한 줄씩 읽어옵니다.
while(!reader.EndOfStream)
{
    // 한 줄씩 읽어옵니다.
    string line = reader.ReadLine();

    // 읽어온 내용을 콘솔에 출력합니다.
    Console.WriteLine(line);
}

// 마지막으로 StreamReader 객체를 닫아줍니다.
reader.Close();

```

## 딥 다이브

StreamReader 클래스는 파일을 읽어오는 기능을 제공하는 클래스입니다. 파일을 읽어올 때 사용하는 메소드는 Read, ReadLine, ReadToEnd 등이 있으며, 각각의 메소드는 다양한 방식으로 파일을 읽어오는데 사용됩니다. 또한 파일을 다 읽어오지 않았을 때 사용할 수 있는 Peek 메소드도 있어서 더욱 유용하게 사용할 수 있습니다.

## See Also

- [](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [](https://www.c-sharpcorner.com/article/read-file-by-using-streamreader-in-C-Sharp/)
- [](https://www.azybao.com/using-streamreader-to-read-file-in-c/?lang=en)