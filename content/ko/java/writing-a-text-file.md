---
title:                "텍스트 파일 작성하기"
html_title:           "Java: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

문자 파일을 작성하는 이유는 데이터를 저장하고 불러오는 데 유용하기 때문입니다.

## 작성 방법

문자 파일을 작성하려면 먼저 `FileWriter` 클래스를 호출해야 합니다. 그 후에 `write()` 메서드를 사용하여 문자열을 파일에 씁니다.

```Java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriter {

	public static void main(String[] args) {
		try {
			// 문자 파일 생성
			FileWriter writer = new FileWriter("myTextFile.txt");
			// 문자열 쓰기
			writer.write("안녕하세요! 저는 자바를 공부하고 있어요.");
			// writer 리소스 해제
			writer.close();
		} catch (IOException e) {
			System.out.println("파일을 작성할 수 없습니다.");
			e.printStackTrace();
		}
	}
}
```

위 코드를 실행하면 현재 디렉토리에 `myTextFile.txt` 파일이 생성되고, 파일 안에는 "안녕하세요! 저는 자바를 공부하고 있어요."라는 문자열이 저장될 것입니다.

## 디프 다이브

자바에서 문자 파일을 작성하는 방법은 `FileWriter` 클래스를 사용하는 것이 일반적입니다. `FileWriter`는 파일을 쓸 때 사용하는 `Writer` 클래스의 하위 클래스입니다. `write()` 메서드를 실행하면 메모리 버퍼에 있는 데이터가 파일에 쓰여집니다. 버퍼를 이용하므로 파일에 데이터가 즉시 쓰이지 않고, `flush()` 메서드가 실행될 때까지 기다리게 됩니다.

## 참고 자료

- [자바 FileWriter 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [자바 Writer 클래스](https://docs.oracle.com/javase/8/docs/api/java/io/Writer.html)