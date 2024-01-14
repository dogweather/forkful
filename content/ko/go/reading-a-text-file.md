---
title:                "Go: 텍스트 파일 읽기."
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 중요한 이유는 많이 있지만, 주로 데이터 처리나 파일 입출력과 같은 작업을 할 때 많이 사용됩니다. 이를 통해 우리는 컴퓨터에 저장된 데이터를 읽고 처리할 수 있습니다.

## 방법

텍스트 파일을 읽는 방법은 Go 언어에서 제공하는 `bufio` 패키지를 사용하면 간단히 처리할 수 있습니다. 먼저, 읽을 파일을 열기 위해 `Open()` 함수를 사용합니다. 파일의 경로는 `strings` 패키지의 `Join()` 함수를 이용해 문자열로 만들어 줍니다. 다음으로 `bufio` 패키지의 `NewScanner()` 함수를 호출해 파일 변수를 스캐너로 변환합니다. 스캐너 변수는 `Scan()` 함수를 호출해 파일을 한 줄씩 읽을 수 있습니다. 예제 코드는 다음과 같습니다.

```Go
f, err := os.Open(strings.Join([]string{"dir", "file.txt"}, "/"))

if err != nil {
    log.Fatal(err)
}

scanner := bufio.NewScanner(f)

for scanner.Scan() {
    fmt.Println(scanner.Text())
}

if err := scanner.Err(); err != nil {
    log.Fatal(err)
}
```

위의 코드를 실행하면 `file.txt` 파일의 내용이 한 줄씩 출력됩니다.

## 심층 분석

파일을 읽는 과정에서 주의해야 할 점은 파일을 읽을 때 발생하는 오류를 체크해야 합니다. 위 코드에서 사용된 `err` 변수를 확인해서 파일을 올바르게 읽었는지를 판단할 수 있습니다. 또한, Go 언어에서는 기본적으로 UTF-8으로 인코딩된 문자열을 처리하기 때문에 파일의 인코딩 방식에 따라 `bufio` 패키지의 `NewScanner()` 함수에 인자를 추가해야 하는 경우도 있습니다.

## 참고 자료

- [Go 언어의 bufio 패키지 문서](https://golang.org/pkg/bufio/)
- [Read and write files - Go 언어 공식 문서](https://golang.org/doc/tutorial/reading-files)
- [How to read files in Go - Tutorial by Joe Adams](https://www.youtube.com/watch?v=gfYeJ9nazVA)

# 참고

[See Also](https://gist.github.com/octocat/9257657)