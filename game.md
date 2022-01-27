# Erlang Game 实践

## 环境搭建

windows 10

erlang otp win64_24.1

rebar 3.17.0+build.5087.ref8be7c3d2 on Erlang/OTP 24 Erts 12.1

InteliJ IDEA 2019.3.1

## 项目创建

1.rebar3 new app game

​	并且对rebar3/rebar.config加上{plugins, [rebar3_auto]}.

​	用于后面载入第三方包或者插件使用

2.使用IDEA 打开game项目

![image-20211113234751442](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211113234751442.png)

3.配置rebar3路径

![image-20211114001334131](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211114001334131.png)

4.配置IDEA的编译环境及运行环境

![image-20211114001017234](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211114001017234.png)

5.执行编译

![image-20211114001430338](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211114001430338.png)

6.设置IDEA运行应用

![image-20211114001819346](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211114001819346.png)

7.新增hello world打印并运行

![image-20211114001947065](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211114001947065.png)

## 项目erl调试

1、然后在控制台环境下运行.app打包文件

每次IntelliJ IDEA进行编译后，都会在_build目录下建立出新.app文件，想要使用服务器运行方式可以使用cmd命令行方式来运行，首先进入到指定目录，运行erlang shell环境，并且使用application:start来运行app文件

![image-20211205143921731](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205143921731.png)

如果想正常释放.app模块，则可以使用application:stop来停止，如果想彻底退出shell环境，可以使用init:stop停止

![image-20211205144111346](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205144111346.png)

2、如何查看shell环境下应用程序监视器

在erl17前使用appmon:start启动，erl17之后使用observer:start，就可以查看以下窗口

![image-20211205144308569](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205144308569.png)

![image-20211205144459094](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205144459094.png)

3、escript运行脚本文件

新建test.erl文件，并且实现main/1函数即可，在控制台可以使用escript运行.erl文件即可运行

![image-20211205144750321](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205144750321.png)

![image-20211205144803898](C:\Users\Lron\AppData\Roaming\Typora\typora-user-images\image-20211205144803898.png)