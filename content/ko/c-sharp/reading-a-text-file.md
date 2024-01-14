---
title:    "C#: 텍스트 파일 읽기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것은 프로그래밍에서 중요한 작업 중 하나입니다. 텍스트 파일은 우리가 일상에서 다루는 다양한 정보를 담고 있기 때문에, 텍스트 파일을 읽는 프로그램을 작성하는 것은 매우 유용합니다. 예를 들어, 주소록을 파일로 저장하고 이를 읽어와 특정 정보를 검색하거나, 로그 파일을 분석하여 시스템의 동작을 모니터링 할 수 있습니다.

## 어떻게

아래의 C# 코드 예제를 통해 텍스트 파일을 읽는 방법을 알아보겠습니다. 우선 `StreamReader` 클래스를 사용하여 파일을 열고, `ReadLine` 메서드를 이용하여 한 줄씩 읽어오도록 할 수 있습니다. 이후 `while` 루프를 사용하여 파일을 끝까지 읽은 다음, `Close` 메서드를 호출하여 파일을 닫아주면 됩니다.

```C#
using System.IO;

StreamReader reader = new StreamReader("example.txt"); // 파일 열기
string line;

while((line = reader.ReadLine()) != null) // 파일 끝까지 읽기
{
    Console.WriteLine(line); // 콘솔에 한 줄씩 출력
}

reader.Close(); // 파일 닫기
```

위의 코드를 실행하면 `example.txt` 파일에 있는 모든 내용을 콘솔에 출력하게 됩니다.

## 깊게 파고들기

텍스트 파일을 읽는 것은 매우 간단하지만, 몇 가지 사소한 실수가 큰 오류를 발생시킬 수 있습니다. 예를 들어, 파일을 열고 사용한 뒤에 꼭 파일을 닫아주는 것을 잊어버리거나, 파일의 경로나 파일명을 잘못 입력하는 등의 실수가 발생할 수 있습니다. 또한 파일이 너무 크거나 잘못된 인코딩으로 작성되어 있는 경우에도 문제가 발생할 수 있습니다. 이러한 경우에는 예외처리를 추가하여 프로그램의 안정성을 높여야 합니다.

또한, 텍스트 파일을 읽는 것 외에도 `StreamWriter` 클래스를 사용하여 새로운 텍스트 파일을 생성하고, `File` 클래스를 사용하여 파일을 복사하거나 삭제하는 등의 작업도 가능합니다.

## 참고 자료

- [`StreamReader` 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamreader?view=net-5.0)
- [`File` 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.file?view=net-5.0)
- [C# 파일 입출력 예제](https://www.csharpstudy.com/Tip/Tip-file-io.aspx)