---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
임시 파일을 생성하는 것은 프로그램 실행 동안 정보를 단기적으로 저장하는 방법입니다. 프로그래머들은 디스크 공간을 효율적으로 사용하고 데이터를 안전하게 유지하기 위해 이를 사용합니다.

## 어떻게:
Haskell에서는 시스템.IO.Temp 모듈로 임시 파일을 만들 수 있습니다. 다음은 그 예입니다:

```haskell
import System.IO.Temp
import System.IO

main :: IO ()
main = withSystemTempFile "example.tmp" $
  \path hdl -> do
    hPutStrLn hdl "test line"
    hClose hdl
```
위 코드는 "example.tmp"라는 임시 파일을 만들고, "test line"이라는 텍스트를 저장한 다음에 파일을 닫습니다.

## 심화
### 역사적 맥락
임시 파일은 프로그램이 일시적인 데이터를 저장하고, 실행 중에 접근해야 하는 경우 사용해왔습니다. 이는 디스크 공간 활용 비율을 높이고 데이터 손실을 방지합니다.

### 대안
Haskell에서는 임시 디렉토리를 만들어 데이터를 관리하는 방법도 있습니다. System.Directory 모듈의 'getTemporaryDirectory' 함수를 사용하면 됩니다.

### 실행 세부정보
위에서 보여준 `withSystemTempFile` 함수는 임시 파일을 만들고, 파라미터로 전달된 함수에게 파일 경로와 핸들을 제공합니다. 사용이 끝나면 임시 파일은 시스템에 의해 자동으로 삭제됩니다.

## 추가정보
임시 파일 생성에 관한 더 깊은 이해를 위해 다음 링크를 참조하십시오.
- [Haskell System.IO.Temp Documentation](https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html)
- [Haskell System.IO Documentation](https://www.haskell.org/onlinereport/haskell2010/haskellch33.html)