set :application, "hugo-a-go-go"
set :repository,  "deveo@deveo.com:clojurecup/projects/hugoagogo/repositories/git/hugo-a-go-go"
set :repository_cache,    "#{application}_cache"
set :environment,         "production"
set :runner,              "root"
set :user,                "root"
set :scm_user,            "root"
set :deploy_to,           "/var/www/#{application}"
set :keep_releases,       2
#set :deploy_via,          :remote_cache
set :scm,                 :git
set :use_sudo, false
set :ssh_options, {:forward_agent => true}

role :web, "146.185.148.102"

after 'deploy:setup', :custom_setup

task :custom_setup do
  run "apt-get update"
  run "apt-get install -y --force-yes git build-essential openjdk-7-jre-headless"
  put File.read(File.join(File.dirname(__FILE__), 'lein')), '/usr/bin/lein', :mode => '711'
end

task :update_upstart do
  put File.read(File.join(File.dirname(__FILE__), 'upstart')), '/tmp/upstart', :mode => '644'
  sudo "mv -f /tmp/upstart /etc/init/hugo.conf"
end

task :update_ssh do
  put File.read(File.join(File.dirname(__FILE__), 'authorized_keys')), '/tmp/authorized_keys', :mode => '600'
  sudo "mv -f /tmp/authorized_keys /root/.ssh/authorized_keys"
end

before :deploy, :update_ssh, :update_upstart
after "deploy:restart", "deploy:cleanup"

namespace :deploy do
  task :restart do
    sudo "stop hugo;true"
    sudo "start hugo"
  end
end
