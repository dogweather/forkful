---
title:                "Go: 텍스트 파일 작성하기"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일 작성에 참여하는 이유는 무엇일까요? Go 언어로 쉽고 효율적으로 텍스트 파일을 작성하는 방법을 알아봅시다.

## 어떻게

아래에는 Go 언어로 텍스트 파일을 작성하는 예시 코드와 출력 결과가 포함되어 있습니다.

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	// 텍스트 파일에 작성할 내용
	text := "안녕하세요! 이것은 샘플 텍스트 파일입니다."

	// 파일 생성 및 쓰기 권한 부여
	file, err := os.Create("sample.txt")
	if err != nil {
		fmt.Println(err)
	}
	defer file.Close()

	// 파일에 내용 입력
	byteSlice := []byte(text)
	bytesWritten, err := file.Write(byteSlice)
	if err != nil {
		fmt.Println(err)
	}
	fmt.Printf("성공적으로 %d 바이트를 썼습니다.\n", bytesWritten)

	// 작성한 파일 읽기
	fileData, err := ioutil.ReadFile("sample.txt")
	if err != nil {
		fmt.Println(err)
	}
	fmt.Printf("파일에 저장된 내용: %s\n", string(fileData))
}
```

**출력 결과:**
```
성공적으로 28 바이트를 썼습니다.
파일에 저장된 내용: 안녕하세요! 이것은 샘플 텍스트 파일입니다.
```

## 딥 다이브

텍스트 파일은 컴퓨터에서 가장 기본적인 데이터 저장 방법입니다. 따라서 파일을 생성하고 내용을 입력하는 방법은 모든 프로그래머에게 반드시 알아야 할 필수 기술입니다. 그러나 파일의 내용을 다루는 방법은 파일의 크기, 구조, 텍스트 인코딩 방식에 따라 다르므로 프로젝트의 목적에 맞게 적절한 방법을 선택하여 사용해야 합니다. 또한 파일 생성과 동시에 권한 부여를 신경써야 하며, 파일을 읽거나 쓸 때 발생할 수 있는 에러들도 처리해야 합니다. 따라서 텍스트 파일을 다루는 것은 꽤 복잡한 작업일 수 있습니다. 하지만 놀랍게도 Go 언어는 이런 불편함을 줄여주는 다양한 함수와 메소드를 제공하고 있습니다. 자세한 내용은 공식 문서나 다양한 블로그를 참고해보세요.

# 또 다른 내용

[Go 언어 공식 문서](https://golang.org/doc/)  
[Go 언어에 대한 생활 코딩 블로그](https://m.post.naver.com/my/series/detail.nhn?seriesNo=457095&memberNo=15263361)  
[여러 가지 프로그래밍 언어로 파일 쓰기와 읽기 예제 비교](https://codedragon.tistory.com/8049)  

# 관련 링크

[메타데이터 파일 생성하기](https://github.com/bangseongbeom/Go-Tutorial/blob/master/metadata_file_creation.md)  
[CSV 파일 작성 및 읽기](https://pooreum.github.io/WebDev/csv/)  
[JSON 파일 생성 및 읽기](https://blog.naver.com/sanddeergam/222211740369)