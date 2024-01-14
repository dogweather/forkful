---
title:    "Javascript: 임시 파일 만들기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜 만드는가?

순간적인 파일을 만드는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 임시적으로 작업하는 동안 필요한 임시 파일이 있을 수 있고, 파일을 다운로드할 때 중간 단계로 사용할 수도 있습니다. 어떤 경우에도, 임시 파일을 만들어서 필요한 작업을 수행할 수 있게 됩니다.

# 어떻게 만드는가?

```javascript
// Node.js에서 임시 파일 만들기
const fs = require('fs');
const path = require('path');

// 임시 파일 생성할 경로 설정
const tempPath = path.join(__dirname, 'temp_file.txt');

// 파일 생성
fs.writeFile(tempPath, '임시 파일 내용', (err) => {
    if(err){
        console.log(err);
    } else {
        console.log('임시 파일이 생성되었습니다.');
    }
});

// 임시 파일 삭제
fs.unlink(tempPath, (err) => {
    if(err) {
        console.log(err);
    } else {
        console.log('임시 파일이 삭제되었습니다.');
    }
});
```

만든 임시 파일은 프로젝트 폴더 내에 생성되며, 원하는 위치로 경로를 설정할 수 있습니다. 파일을 생성할 때는 `fs.writeFile()` 메소드를 사용하고, 파일을 삭제할 때는 `fs.unlink()` 메소드를 사용합니다. 이렇게 임시 파일을 만들고 삭제하는 과정을 코드를 통해 살펴보았습니다.

# 깊이 파고들기

임시 파일은 컴퓨터에서 일시적으로 사용되는 파일로, 프로그램이 실행되는 동안 필요한 데이터를 임시저장해두는 역할을 합니다. 그래서 보통은 프로그램이 종료되면 자동으로 삭제가 됩니다. 이렇게 임시 파일이 생성되고 사용될 때, 사용자는 따로 신경 쓸 필요 없이 프로그램을 편리하게 사용할 수 있게 됩니다.

# 참고 자료

- [Node.js 공식 문서 - fs.writeFile()](https://nodejs.org/api/fs.html#fs_fs_writefile_file_data_options_callback)
- [Node.js 공식 문서 - fs.unlink()](https://nodejs.org/api/fs.html#fs_fs_unlink_path_callback)