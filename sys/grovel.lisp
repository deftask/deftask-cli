(in-package #:deftask-sys)

(include "sys/errno.h")
(include "sys/types.h")
(include "sys/stat.h")
(include "termios.h")
(include "unistd.h")
(include "fcntl.h")
(include "signal.h")

(ctype pid "pid_t")
(ctype mode "mode_t")
(cstruct termios "struct termios")

(constantenum error-number
              ((:child "ECHILD")))

(constantenum oflag
              ((:o-rdonly "O_RDONLY"))
              ((:o-wronly "O_WRONLY"))
              ((:o-rdwr "O_RDWR"))
              ((:o-creat "O_CREAT"))
              ((:o-trunc "O_TRUNC"))
              ((:o-append "O_APPEND")))

(constantenum mode
              ((:s-irwxu "S_IRWXU"))
              ((:s-irusr "S_IRUSR"))
              ((:s-iwusr "S_IWUSR"))
              ((:s-ixusr "S_IXUSR"))
              ((:s-irwxg "S_IRWXG"))
              ((:s-irgrp "S_IRGRP"))
              ((:s-iwgrp "S_IWGRP"))
              ((:s-ixgrp "S_IXGRP"))
              ((:s-irwxo "S_IRWXO"))
              ((:s-iroth "S_IROTH"))
              ((:s-iwoth "S_IWOTH"))
              ((:s-ixoth "S_IXOTH"))
              ((:s-isuid "S_ISUID"))
              ((:s-isgid "S_ISGID"))
              ((:s-isvtx "S_ISVTX")))

(constantenum sig
              ((:hup "SIGHUP"))
              ((:int "SIGINT"))
              ((:quit "SIGQUIT"))
              ((:ill "SIGILL"))
              ((:trap "SIGTRAP"))
              ((:abrt "SIGABRT"))
              ((:emt "SIGEMT"))
              ((:fpe "SIGFPE"))
              ((:kill "SIGKILL"))
              ((:bus "SIGBUS"))
              ((:segv "SIGSEGV"))
              ((:sys "SIGSYS"))
              ((:pipe "SIGPIPE"))
              ((:alrm "SIGALRM"))
              ((:term "SIGTERM"))
              ((:urg "SIGURG"))
              ((:stop "SIGSTOP"))
              ((:tstp "SIGTSTP"))
              ((:cont "SIGCONT"))
              ((:chld "SIGCHLD"))
              ((:ttin "SIGTTIN"))
              ((:ttou "SIGTTOU"))
              ((:io "SIGIO"))
              ((:xcpu "SIGXCPU"))
              ((:xfsz "SIGXFSZ"))
              ((:vtalrm "SIGVTALRM"))
              ((:prof "SIGPROF"))
              ((:winch "SIGWINCH"))
              ((:info "SIGINFO"))
              ((:usr1 "SIGUSR1"))
              ((:usr2 "SIGUSR2")))

(constant (+sig-dfl+ "SIG_DFL"))
(constant (+sig-ign+ "SIG_IGN"))
(constant (+sig-err+ "SIG_ERR"))

(constant (+tcsadrain+ "TCSADRAIN"))
