---
title:                "Java: 텍스트 파일 쓰기"
simple_title:         "텍스트 파일 쓰기"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

자바 프로그래밍을 할 때 텍스트 파일을 작성하는 것의 중요성은 무엇일까요?

## 어떻게

텍스트 파일을 작성하는 방법은 다양합니다. 가장 간단한 방법은 FileWriter 클래스를 사용하는 것입니다. 아래의 예제 코드를 통해 더 자세한 방법을 알아보세요.

```Java
import java.io.FileWriter;

public class TextFileWriter {
	public static void main(String[] args) {
		try {
			// FileWriter 객체 생성
			FileWriter writer = new FileWriter("sample.txt");
			
			// 파일에 쓸 내용 작성
			writer.write("안녕하세요, 이것은 샘플 파일입니다.");
			
			// 파일 닫기
			writer.close();
			
			System.out.println("파일이 성공적으로 작성되었습니다.");
		} catch (Exception e) {
			System.out.println("파일 작성 중 오류가 발생하였습니다.");
			e.printStackTrace();
		}
	}
}
```

위 코드를 실행하면 현재 작업폴더에 `sample.txt` 파일이 생성되고 해당 내용이 작성됩니다. 만약 이미 같은 이름의 파일이 있으면 기존 파일의 내용이 덮어쓰여집니다.

## 딥 다이브

텍스트 파일을 작성할 때 주의해야 할 몇 가지 중요한 사항이 있습니다.

- 파일의 경로를 제대로 지정해야 합니다. 파일의 생성 위치를 정확히 알고 있어야 잘못된 경로로 인해 파일이 생성되지 않습니다.
- 파일을 쓴 후에는 `close()` 메소드를 호출하여 파일을 닫아야 합니다. 이 작업을 하지 않으면 파일이 제대로 저장되지 않을 수 있습니다.
- 파일을 생성하는 대신 이미 존재하는 파일에 내용을 추가하고 싶다면 FileWriter의 두 번째 인자로 `true`를 넘겨줍니다.

## 참고

- [Java FileWriter 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/io/FileWriter.html)
- [Java File 클래스 문서](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Java 파일 읽고 쓰기 방법 알아보기](https://coding-factory.tistory.com/245)