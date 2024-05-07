function tdt_export_small(inputpath,outputpath,sdkpath,id,names,N)
    %adding TDTbin2mat to Matlab
    SDKPATH = sdkpath; % '~/Documents/Matlab/custom_libs/';
    addpath(genpath(SDKPATH));

    %disp('job done')
    % Parses a project folder recorded with TDT DAQ / Synapse and returns a data
    % matrix where columns correspond to channels listed in names.
    % 2019-02-01. Leonardo Molina.

    % read data
    raw = TDTbin2mat(inputpath);
    nChannels = numel(names);
    nSamples = numel(raw.streams.(names{1}).data);
    time = (1:length(raw.streams.(names{1}).data))/raw.streams.(names{1}).fs; % time (for all samples)
    %frequency = raw.streams.(names{1}).fs;

    % downsample:
    for c = 1:nChannels
      raw.streams.(names{c}).data = arrayfun(@(i)mean(raw.streams.(names{c}).data(i:i+N-1)),1:N:length(raw.streams.(names{c}).data)-N+1);
    end
    % cut length:
    all_mins = zeros(1,nChannels);
    for c = 1:nChannels
       all_mins(c) = numel(raw.streams.(names{c}).data);
    end
    nSamples2 = min(all_mins);
    % nSamples2 = min(numel(raw.streams.(names{1}).data),numel(raw.streams.(names{2}).data));
    data = NaN(nSamples2, nChannels + 1);
    for c = 1:nChannels
       data(:, c + 1) = double(raw.streams.(names{c}).data(1:nSamples2));
    end
    %Decimate time array and match length to demodulated stream
    time = time(1:N:end);
   % time = time(1:length(raw.streams.(names{c}).data));
    time = time(1:nSamples2);
    data(:, 1) = double(time);
    % write to file
    out = strcat(outputpath,id,"_f",N,'_export_small.txt');
    dlmwrite(out, data, 'delimiter','\t','newline','pc','precision',13);
   % writematrix(data,out,'Delimiter','tab')
   % dlmwrite('/beegfs/scratch/bruening_scratch/lsteuernagel/data/fiberPhotometry/fromCorinna/040820BAU0000587_flat.txt', data, 'delimiter','\t','newline','pc','precision',13);
end

% /beegfs/bin/matlab_2014b -nodisplay -nojvm -r 'tdt_export inputpath outputpath names; exit;'
