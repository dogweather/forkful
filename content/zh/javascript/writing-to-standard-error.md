---
title:                "Javascript: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么要记录错误信息

当我们编写代码时，难免会遇到错误。尤其是在大型项目中，出现bug会耗费大量的时间去定位和解决。这时候，记录错误信息就变得非常重要。通过记录错误信息，我们可以快速地定位和解决bug，提高代码质量和效率。

# 如何记录错误信息

在Javascript中，我们可以使用console.error()方法来向标准错误流写入错误信息。例如：

```Javascript
console.error("发生了一个错误");
```

输出结果为：

```Javascript
发生了一个错误
```

我们也可以在错误信息中添加变量，来更详细地描述错误的来源。例如：

```Javascript
let number = 5;
console.error(`无效的数字：${number}`);
```

输出结果为：

```Javascript
无效的数字：5
```

# 深入了解错误信息记录

除了使用console.error()方法外，我们还可以使用try-catch语句来捕获和处理错误。例如：

```Javascript
try{
    // 需要执行的代码
}catch(error){
    console.error(error);
}
```

这样的话，即使发生了错误，程序也不会终止运行，而是会在catch语句中输出错误信息。

另外，我们还可以自定义错误，并使用throw语句抛出错误。例如：

```Javascript
function multiply(a, b){
    if(typeof(a) !== "number" || typeof(b) !== "number"){
        throw new Error("输入的参数必须是数字");
    }else{
        return a * b;
    }
}

// 使用示例
console.log(multiply(2, "3")); // 抛出错误："输入的参数必须是数字"
```

通过自定义错误，我们可以更清晰地指明错误的类型和来源。

# 参考链接

- [Javascript错误处理](https://developer.mozilla.org/zh-CN/docs/Learn/JavaScript/First_steps/What_went_wrong)
- [深入理解try-catch语句](https://www.w3schools.com/js/js_errors.asp)
- [自定义错误](https://www.w3schools.com/js/js_error.asp)

# 参见

- [Javascript基础语法介绍](https://www.runoob.com/js/js-tutorial.html)
- [学习Javascript的5个技巧](https://www.runoob.com/w3cnote/5-tips-for-learning-javascript.html)
- [Markdown教程](https://www.runoob.com/markdown/md-tutorial.html)