---
title:                "텍스트 파일 읽기"
html_title:           "Java: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?: 
텍스트 파일을 읽는 것은 프로그래머에게 있어서 매우 중요한 작업입니다. 텍스트 파일은 데이터를 저장하고 전송하기에 매우 유용한 방식입니다. 따라서 텍스트 파일을 읽고 처리하는 프로그래밍 기술을 배우는 것은 필수적입니다.

## 하는 법:
```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadTextFile {

	public static void main(String[] args) throws IOException {
		String fileName = "textfile.txt"; // 읽을 파일의 경로와 이름
		FileReader fileReader = new FileReader(fileName); // FileReader 객체 생성
		BufferedReader bufferedReader = new BufferedReader(fileReader); // BufferedReader 객체 생성
		String line = null; // 한 줄씩 읽어올 변수
		while ((line = bufferedReader.readLine()) != null) { // 파일의 끝까지 한 줄씩 읽음
			System.out.println(line); // 한 줄씩 출력
		}
		bufferedReader.close(); // BufferedReader 닫기
	}
}
```

위의 코드는 주어진 파일을 한 줄씩 읽어서 화면에 출력하는 간단한 예제입니다. 이 코드를 실행하면 파일의 내용이 출력됩니다.

## 깊이 파헤치기:
텍스트 파일을 읽는 기술은 예전부터 사용되고 있습니다. 초기에는 바이너리 파일로 데이터를 저장하고 전송하는 방식이 많이 사용되었지만, 텍스트 파일의 단순함과 유용성으로 인해 현재 많이 사용됩니다. 다른 방법으로는 스트림을 사용하여 텍스트 파일을 읽을 수 있습니다. 하지만 BufferedReader와 FileReader를 사용하는 방법이 가장 일반적입니다. 

## 더 보기:
- [Java 파일 읽기 예제](https://www.codejava.net/java-se/file-io/read-text-file-in-java)
- [Java 스트림 사용하기](https://docs.oracle.com/javase/tutorial/essential/io/streams.html)