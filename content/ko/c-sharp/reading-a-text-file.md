---
title:                "C#: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것에 드는 시간과 노력은 걸릴 수 있지만, 그것만큼이나 값진 경험이 될 수 있습니다. 다양한 정보를 포함하는 파일을 쉽게 읽을 수 있고 데이터를 추출할 수 있기 때문에 다양한 프로그래밍 프로젝트에서 텍스트 파일을 읽는 것은 매우 유용합니다.

## 하나

텍스트 파일을 읽는 것은 C#에서 매우 쉽습니다. 먼저, `StreamReader` 클래스를 사용하여 파일을 오픈하고, `ReadLine()` 메소드를 사용하여 파일의 각 줄을 읽을 수 있습니다. 아래는 간단한 예제 코드입니다.

```C#
StreamReader reader = new StreamReader("example.txt"); // 파일 오픈

string line;

while ((line = reader.ReadLine()) != null) // 한 줄씩 읽음
{
    Console.WriteLine(line); // 콘솔에 출력
}

reader.Close(); // 파일 닫기
```

위 예제 코드에서 `example.txt` 파일으로부터 한 줄씩 읽어서 콘솔에 출력하는 것을 볼 수 있습니다. 실제 프로젝트에서는 읽은 데이터를 변수에 저장하거나 다른 형태로 가공할 수 있습니다.

## 깊이 파고들기

텍스트 파일을 읽는 데에는 몇 가지 주의해야 할 점이 있습니다. 첫째로, 파일의 인코딩 방식을 정확히 알아야 합니다. 대부분의 경우 UTF-8 인코딩을 사용하지만, 다른 인코딩 방식을 사용하는 경우도 있을 수 있습니다. 둘째로, 파일을 오픈하기 전에 파일이 존재하는지 확인해야 합니다. 만약 파일이 존재하지 않는 경우 예외 처리를 해주어야 합니다.

또한, `using` 키워드를 사용하여 `StreamReader` 객체를 오픈하면 자동으로 파일을 닫아줍니다. 이를 이용하여 파일을 읽은 후에는 `using` 키워드를 사용하는 것을 추천합니다.

## 참고

- [C# StreamReader 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader)
- [C# 파일 읽기/쓰기 예제 코드](https://www.c-sharpcorner.com/blogs/reading-and-writing-text-files-using-c-sharp1)
- [C# 파일 관리 예외 처리하기](https://www.c-sharpcorner.com/blogs/file-management-exception-handling-in-c-sharp1)